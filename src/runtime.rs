use std::{
    env,
    fmt::Display,
    fs::{self, OpenOptions},
    io::{BufWriter, Write},
    path::PathBuf,
};

use crate::{
    expr::Expr,
    lexer::{self, CommandKind, Lexer, Loc, StrategyKind, TokenKind},
    rule::{ApplyAll, ApplyCheck, ApplyDeep, ApplyFirst, ApplyNth, Rule, Strategy},
};
use anyhow::Result;
use crossterm::{event::Event, style::Stylize};
use linked_hash_map::LinkedHashMap;

pub struct Runtime {
    rules: LinkedHashMap<String, Rule>,
    apply_history: Vec<Expr>,
    undo_history: Vec<Expr>,
    shape_stack: Vec<Expr>,
    shaping_rule: Option<(String, Expr)>,
    pub quit: bool,
    pub verbosity: Verbosity,
    pub interaction_hook: Option<Box<dyn Fn(Box<dyn Interaction>) -> InteractionResult>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Verbosity {
    Silent,
    Normal,
    Verbose,
}

#[derive(Debug, thiserror::Error)]
//#[error("[{location}] Runtime error: {kind} {message}.")]
pub struct RuntimeError {
    pub message: Option<String>,
    pub kind: RuntimeErrorKind,
    pub location: Loc,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(message) = &self.message {
            write!(
                f,
                "[{}] Runtime error: {}, {}",
                self.location, self.kind, message
            )
        } else {
            write!(f, "[{}] Runtime error: {}", self.location, self.kind)
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RuntimeErrorKind {
    #[error("Invalid command {0}")]
    InvalidCommand(String),
    #[error("Invalid strategy {0}")]
    InvalidStrategy(String),
    #[error("Expected rule name")]
    ExpectedRuleName,
    #[error("No rule named {0}")]
    RuleDoesNotExist(String),
    #[error("Expected rule expression or anonymous rule")]
    ExpectedRuleExprOrAnon,
    #[error("No shape defined")]
    NoShape,
    #[error("Already shaping")]
    AlreadyShaping,
    #[error("Nothing to undo")]
    NothingToUndo,
    #[error("Nothing to redo")]
    NothingToRedo,
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("File error")]
    FileError,
    #[error("Parse error")]
    ParseError,
    #[error("Apply error")]
    ApplyError,
    #[error("Unexpected token")]
    UnexpectedToken,
}
use RuntimeErrorKind::*;

impl RuntimeErrorKind {
    pub fn message(self, message: String, location: Loc) -> RuntimeError {
        RuntimeError {
            location,
            kind: self,
            message: Some(message),
        }
    }

    pub fn err(self, loc: Loc) -> RuntimeError {
        RuntimeError {
            message: None,
            kind: self,
            location: loc,
        }
    }
}

trait IntoRuntimeError<T> {
    fn runtime(self, kind: RuntimeErrorKind, location: Loc) -> Result<T, RuntimeError>;
}

impl<T, U: Display> IntoRuntimeError<T> for Result<T, U> {
    fn runtime(self, kind: RuntimeErrorKind, location: Loc) -> Result<T, RuntimeError> {
        match self {
            Ok(s) => Ok(s),
            Err(e) => Err(RuntimeError {
                message: Some(e.to_string()),
                kind,
                location,
            }),
        }
    }
}

#[allow(unused)]
pub enum InteractionResult {
    String(String),
    Int(isize),
    UInt(usize),
    Float(f64),
    Option(Option<Box<InteractionResult>>),
}

pub trait Interaction {
    fn display(&self) -> String;
    /// Returns Some(value) when the interaction is complete, or none to continue
    fn on_event(&mut self, input: Event) -> Option<InteractionResult>;

    fn on_complete(&mut self, _result: &InteractionResult) {}
}

pub struct SelectOne {
    pub prompt: String,
    pub options: Vec<String>,
    pub selected: usize,
}

impl SelectOne {
    pub fn new(prompt: String, options: Vec<String>) -> Self {
        Self {
            prompt,
            options,
            selected: 0,
        }
    }
}

impl Interaction for SelectOne {
    fn display(&self) -> String {
        let mut out = Vec::new();
        out.push(format!("{}\n", self.prompt).bold().to_string());
        for (i, option) in self.options.iter().enumerate() {
            if i == self.selected {
                out.push(format!("> {}\n", option).bold().to_string());
            } else {
                out.push(format!("| {}\n", option).reset().to_string());
            }
        }
        out.join(" ")
    }

    fn on_event(&mut self, input: Event) -> Option<InteractionResult> {
        use crossterm::event::KeyCode::*;
        match input {
            Event::Key(key) => match key.code {
                Left | Up => {
                    if self.selected > 0 {
                        self.selected -= 1;
                    } else {
                        self.selected = self.options.len() - 1;
                    }
                    None
                }
                Right | Down => {
                    if self.selected < self.options.len() - 1 {
                        self.selected += 1;
                    } else {
                        self.selected = 0;
                    }
                    None
                }
                Enter => Some(InteractionResult::UInt(self.selected)),
                _ => None,
            },
            _ => None,
        }
    }

    fn on_complete(&mut self, _result: &InteractionResult) {}
}

pub struct StepResult {
    pub results: Option<Vec<Vec<String>>>,
    pub cmd_for_each: bool,
    pub trigger: Option<Box<dyn Interaction>>,
    pub clear: bool,
}

impl StepResult {
    pub fn new(results: Vec<Vec<String>>, cmd_for_each: bool, clear: bool) -> Self {
        Self {
            results: Some(results),
            cmd_for_each,
            clear,
            trigger: None,
        }
    }
    pub fn empty() -> Self {
        Self {
            results: None,
            cmd_for_each: false,
            clear: false,
            trigger: None,
        }
    }

    pub fn clear() -> Self {
        Self {
            results: None,
            cmd_for_each: false,
            clear: true,
            trigger: None,
        }
    }

    pub fn with_results(results: Vec<Vec<String>>) -> Self {
        Self {
            results: Some(results),
            cmd_for_each: false,
            clear: false,
            trigger: None,
        }
    }
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            rules: LinkedHashMap::new(),
            apply_history: Vec::new(),
            undo_history: Vec::new(),
            shape_stack: vec![],
            shaping_rule: None,
            verbosity: Verbosity::Silent,
            quit: false,
            interaction_hook: None,
        }
    }

    pub fn show_shape(&self) -> Result<String> {
        if let Some(shape) = &self.shape_stack.last() {
            Ok(format!("{}", shape))
        } else {
            Err(NoShape.into())
        }
    }

    fn cmd_use(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let (file_name, loc) = match lexer.next() {
            Some(t) => match t {
                lexer::Token {
                    kind: TokenKind::Ident,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::String,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::Path,
                    text,
                    loc,
                    ..
                } => (text, loc),
                invalid => {
                    return Err(InvalidCommand(format!("{}", invalid.text.red().bold()))
                        .message("expected path".into(), invalid.loc))
                }
            },
            None => {
                return Err(
                    UnexpectedEOF.message(format!("Expected file name"), lexer.current_loc())
                )
            }
        };
        let path = PathBuf::from(&file_name);
        if !path.exists() {
            return Err(FileError.message(
                format!(
                    "{} {}",
                    path.to_str().unwrap(),
                    "does not exist".red().bold()
                ),
                loc,
            ));
        }
        if path.is_dir() {
            return Err(FileError.message(
                format!("{} is a directory", path.to_str().unwrap().red().bold(),),
                loc,
            ));
        }
        let contents = fs::read_to_string(&path)
            .map_err(|e| {
                anyhow::anyhow!("{}: {}", path.to_str().unwrap(), e.to_string().red().bold())
            })
            .runtime(FileError, lexer.current_loc())?;
        let mut lexer = lexer::Lexer::new(contents.chars().peekable());
        let mut res = vec![];
        let old_verbosity = self.verbosity;
        self.verbosity = Verbosity::Silent;
        while !lexer.exhausted {
            res.push(self.step(&mut lexer)?);
        }
        self.verbosity = old_verbosity;

        let res = res
            .iter_mut()
            .filter_map(|s| s.results.take())
            .flatten()
            .collect::<Vec<_>>();

        if res.is_empty() {
            Ok(StepResult::empty())
        } else {
            Ok(StepResult::with_results(res))
        }
    }

    fn help(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let color = |s: &'static str| s.dark_cyan().bold();
        let underline = |s: &'static str| s.underlined();
        return Ok(StepResult::with_results(vec![vec![format!(
            "Commands:
         {rule} {rule_name} <rule>; - define a rule
         {apply} {rule_name}        - apply a rule to the shape
         {apply} {rule} <rule>;       - apply an anonymous rule to the shape
         {shape} <shape>            - define the shape
         {shape}                    - print current shape
         {done}                     - finish the shape (clear current shape)
         {undo}                     - undo the last apply
         {redo}                     - redo the last undo
         {help}                     - print this help message

     Example:
         {rule} {swap} swap(X(A, B)) = X(B, A);
         {rule} {rot} rot(triple(A, B, C)) = triple(C, A, B);

         {shape} swap(pair(f(a), g(b)))
             {apply} {swap}
             {apply} {rule} pair(A, B) = rot(triple(A, B, c));
             {apply} {rot}
         {done}
                    ",
            rule = color("rule"),
            rule_name = underline("<rule_name>"),
            rot = underline("rot"),
            swap = underline("swap"),
            shape = color("shape"),
            done = color("done"),
            undo = color("undo"),
            redo = color("redo"),
            help = color("help"),
            apply = color("apply")
        )]]));
    }

    fn cmd_undo(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        if let Some(shape) = self.apply_history.pop() {
            self.undo_history.push(std::mem::replace(
                &mut self.shape_stack.last_mut().unwrap(),
                shape,
            ));
            Ok(StepResult::with_results(vec![vec![format!(
                "{}",
                self.shape_stack.last().unwrap()
            )]]))
        } else {
            return Err(NothingToUndo.err(lexer.current_loc()));
        }
    }

    fn cmd_redo(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        if let Some(shape) = self.undo_history.pop() {
            self.apply_history.push(std::mem::replace(
                &mut self.shape_stack.last_mut().unwrap(),
                shape,
            ));
            Ok(StepResult::with_results(vec![vec![format!(
                "{}",
                self.shape_stack.last().unwrap()
            )]]))
        } else {
            return Err(NothingToRedo.err(lexer.current_loc()));
        }
    }

    fn cmd_save(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        enum SaveType {
            All,
            Commands,
            Rules,
        }

        let (save_type, loc) = match lexer.next() {
            Some(tok) => match tok.kind {
                TokenKind::Rules => (SaveType::Rules, tok.loc),
                TokenKind::Commands => (SaveType::Commands, tok.loc),
                TokenKind::Strategy(StrategyKind::ApplyAll) => (SaveType::All, tok.loc),
                _ => {
                    return Err(
                        InvalidCommand(format!("{}", tok.text.red().bold())).message(
                            format!("Expected save type (all, commands, or rules)"),
                            tok.loc,
                        ),
                    )
                }
            },
            None => {
                return Err(UnexpectedEOF.message(
                    format!("Expected save type (all, commands, rules) and file name"),
                    lexer.current_loc(),
                ))
            }
        };

        let file_name = match lexer.next() {
            Some(t) => match t {
                lexer::Token {
                    kind: TokenKind::Ident,
                    text,
                    ..
                } => text,
                lexer::Token {
                    kind: TokenKind::String,
                    text,
                    ..
                } => text,
                lexer::Token {
                    kind: TokenKind::Path,
                    text,
                    ..
                } => text,
                invalid => {
                    return Err(InvalidCommand(format!("{}", invalid.text.red().bold()))
                        .message(format!("Expected file name"), invalid.loc))
                }
            },
            None => return Err(UnexpectedEOF.message(format!("Expected file name"), loc)),
        };
        let path = PathBuf::from(&file_name);
        let mut opt = OpenOptions::new();
        if path.exists() && path.is_file() {
            if let Some(hook) = self.interaction_hook.as_mut() {
                let interaction = SelectOne::new(
                    format!(
                        "File {} already exists! What would you like to do?",
                        path.to_str().unwrap().red()
                    ),
                    vec![
                        "Overwrite".to_string(),
                        "Append".to_string(),
                        "Cancel".to_string(),
                    ],
                );
                match hook(box interaction) {
                    InteractionResult::UInt(i) => {
                        if i == 0 {
                            // overwrite
                            opt.read(true).write(true).truncate(true);
                        } else if i == 1 {
                            // append
                            opt.read(true).append(true);
                        } else {
                            return Err(anyhow::anyhow!(
                                "File {} already exists",
                                path.to_str().unwrap()
                            ))
                            .runtime(FileError, lexer.current_loc());
                        }
                    }
                    _ => unreachable!(),
                }
            }
        } else if !path.exists() {
            fs::File::create(&path)
                .map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot create file: {}, {}",
                        path.to_str().unwrap(),
                        e.to_string().red().bold()
                    )
                })
                .runtime(FileError, lexer.current_loc())?;
            opt.write(true).read(true).truncate(true);
        } else {
            return Err(FileError.message(
                format!("{} is a directory", path.to_str().unwrap().red().bold(),),
                loc,
            ));
        };

        let file = opt.open(&path).map_err(|e| {
            FileError.message(
                format!(
                    "Cannot open file: {}, {}",
                    path.to_str().unwrap(),
                    e.to_string().red().bold()
                ),
                lexer.current_loc(),
            )
        })?;
        let mut writer = BufWriter::new(file);

        let res = match save_type {
            SaveType::All => {
                for (name, rule) in self.rules.iter() {
                    writer
                        .write_fmt(format_args!("rule {} {}\n", name, rule))
                        .runtime(FileError, lexer.current_loc())?;
                    writer.flush().runtime(FileError, lexer.current_loc())?;
                }
                format!(
                    "Saved {} rules and {} commands to {}",
                    self.rules.len(),
                    0,
                    path.to_str().unwrap()
                )
            }
            SaveType::Commands => todo!(),
            SaveType::Rules => {
                for (name, rule) in self.rules.iter() {
                    writer
                        .write_fmt(format_args!("rule {} {}\n", name, rule))
                        .runtime(FileError, lexer.current_loc())?;
                    writer.flush().runtime(FileError, lexer.current_loc())?;
                }
                format!(
                    "Saved {} rules to {}",
                    self.rules.len(),
                    path.to_str().unwrap()
                )
            }
        };
        writer.flush().runtime(FileError, loc)?;
        Ok(StepResult::with_results(vec![vec![res]]))
    }

    fn cmd_run(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let (file_name, loc) = match lexer.next() {
            Some(t) => match t {
                lexer::Token {
                    kind: TokenKind::Ident,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::String,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::Path,
                    text,
                    loc,
                    ..
                } => (text, loc),
                invalid => {
                    return Err(InvalidCommand(format!("{}", invalid.text.red().bold()))
                        .message("expected path".into(), invalid.loc))
                }
            },
            None => {
                return Err(
                    UnexpectedEOF.message(format!("Expected file name"), lexer.current_loc())
                )
            }
        };
        let path = PathBuf::from(&file_name);
        if !path.exists() {
            return Err(FileError.message(
                format!("{} does not exist", path.to_str().unwrap().red().bold(),),
                loc,
            ));
        }
        if path.is_dir() {
            return Err(FileError.message(
                format!("{} is a directory", path.to_str().unwrap().red().bold(),),
                loc,
            ));
        }
        let contents = fs::read_to_string(&path)
            .map_err(|e| {
                anyhow::anyhow!("{}: {}", path.to_str().unwrap(), e.to_string().red().bold())
            })
            .runtime(FileError, lexer.current_loc())?;
        let mut lexer = lexer::Lexer::new(contents.chars().peekable())
            .with_file_name(path.file_name().unwrap().to_str().unwrap().to_string());
        let mut res = vec![];
        while !lexer.exhausted {
            res.push(self.step(&mut lexer)?);
        }

        let res = res
            .iter_mut()
            .filter_map(|s| s.results.take())
            .flatten()
            .collect::<Vec<_>>();

        if res.is_empty() {
            Ok(StepResult::empty())
        } else {
            Ok(StepResult::with_results(res))
        }
    }

    fn cmd_done(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        if let Some(shape) = self.shape_stack.pop() {
            if let Some((name, head)) = self.shaping_rule.take() {
                self.rules.insert(name.clone(), Rule { head, body: shape });
                Ok(StepResult::with_results(vec![vec![format!(
                    "Rule {} defined",
                    name.yellow(),
                )]]))
            } else {
                Ok(StepResult::with_results(vec![vec![format!(
                    "{} {}",
                    shape.to_string().green(),
                    "\u{2714}".green().bold()
                )]]))
            }
        } else {
            return Err(NoShape.err(lexer.current_loc()));
        }
    }

    fn cmd_anon_rule(
        &mut self,
        mut lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let head = Expr::parse(&mut lexer).runtime(ParseError, lexer.current_loc())?;
        let rule = if lexer.next_if(|x| x.kind == TokenKind::Equals).is_some() {
            let body = Expr::parse(&mut lexer).runtime(ParseError, lexer.current_loc())?;

            Rule { head, body }
        } else {
            let bad = lexer.next_token();
            return Err(
                UnexpectedToken.message(format!("anon rule expected '=', got {}", bad), bad.loc)
            );
        };
        if !lexer.next_if(|x| x.kind == TokenKind::Bar).is_some() {
            let bad = lexer.next_token();
            return Err(UnexpectedToken.message(format!("Expected '|', got {}", bad), bad.loc));
        }
        let reversed = lexer.next_if(|x| x.kind == TokenKind::Bang).is_some();
        let (strategy, n) = match lexer.next() {
            Some(lexer::Token {
                kind: TokenKind::Strategy(strategy),
                text,
                ..
            }) => (strategy, text),
            Some(invalid) => {
                return Err(InvalidStrategy(invalid.text).err(invalid.loc));
            }
            None => {
                return Err(
                    UnexpectedEOF.message(format!("Expected strategy name"), lexer.current_loc())
                );
            }
        };
        let res = self
            .do_apply(&if reversed { rule.reverse() } else { rule }, strategy, n)
            .runtime(ApplyError, lexer.current_loc())?;
        if let Some(res) = res {
            Ok(StepResult::new(
                if self.verbosity == Verbosity::Silent {
                    vec![]
                } else {
                    vec![res.0]
                },
                res.1,
                res.2,
            ))
        } else {
            Ok(StepResult::empty())
        }
    }

    fn cmd_all_rules(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        //lexer.catchup();
        if !lexer.next_if(|x| x.kind == TokenKind::Bar).is_some() {
            let bad = lexer.next_token();
            return Err(UnexpectedToken.message(format!("Expected '|', got {}", bad), bad.loc));
        }
        let reverse = lexer.next_if(|x| x.kind == TokenKind::Bang).is_some();
        let (strategy, n) = match lexer.next() {
            Some(lexer::Token {
                kind: TokenKind::Strategy(strategy),
                text,
                ..
            }) => (strategy, text),
            Some(invalid) => {
                return Err(InvalidStrategy(invalid.text).err(invalid.loc));
            }
            None => {
                return Err(
                    UnexpectedEOF.message(format!("Expected strategy name"), lexer.current_loc())
                );
            }
        };
        let mut r = vec![];
        self.rules
            .clone()
            .iter()
            .map(|(name, rule)| {
                (
                    name,
                    if reverse {
                        rule.reverse()
                    } else {
                        rule.clone()
                    },
                )
            })
            .map(|(name, rule)| {
                let sub_result = self.do_apply(&rule, strategy.clone(), n.clone())?;
                if let Some(sub_result) = sub_result {
                    let mut new = vec![];
                    if !sub_result.0.is_empty() {
                        new.push(format!("{}:", name.to_owned().green()));
                        new.extend(sub_result.0.iter().map(|s| format!("-> {}", s)));
                    } else {
                        new.push(format!("{}:", name));
                        new.push(format!("-> {}", "no matches".red()))
                    }
                    r.push(new);
                }
                Ok(())
            })
            .collect::<Result<_>>()
            .runtime(ApplyError, lexer.current_loc())?;

        Ok(StepResult::with_results(
            if self.verbosity == Verbosity::Silent {
                vec![]
            } else {
                r
            },
        ))
    }

    fn cmd_def_rule(
        &mut self,
        mut lexer: &mut Lexer<impl Iterator<Item = char>>,
        name: String,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        // Rule definition
        let head = Expr::parse(&mut lexer).runtime(ParseError, lexer.current_loc())?;

        match lexer.next_token().kind {
            TokenKind::OpenBrace | TokenKind::Eof => {
                // Start shaping for rule
                self.shaping_rule = Some((name.clone(), head));
                self.shape_stack
                    .push(self.shaping_rule.as_ref().unwrap().1.clone());
                if self.verbosity == Verbosity::Silent {
                    Ok(StepResult::empty())
                } else {
                    Ok(StepResult::with_results(vec![vec![format!(
                        "{}",
                        self.shape_stack.last().unwrap()
                    )]]))
                }
            }
            TokenKind::Equals => {
                self.rules.insert(
                    name.clone(),
                    Rule {
                        head,
                        body: Expr::parse(&mut lexer).runtime(ParseError, lexer.current_loc())?,
                    },
                );
                Ok(StepResult::with_results(vec![vec![format!(
                    "Rule {} defined",
                    name.yellow()
                )]]))
            }
            _ => {
                let invalid = lexer.next_token();
                return Err(UnexpectedToken.message(
                    format!("Expected open brace or equals, found {}", invalid),
                    invalid.loc,
                ));
            }
        }
    }

    fn cmd_apply_named_rule(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
        name: String,
    ) -> Result<StepResult, RuntimeError> {
        // Apply rule
        lexer.catchup();
        if self.shape_stack.last().is_some() {
            let reverse = lexer.next_if(|t| t.kind == TokenKind::Bang).is_some();

            let (strategy, n) = match lexer.next() {
                Some(lexer::Token {
                    kind: TokenKind::Strategy(strategy),
                    text,
                    ..
                }) => (strategy, text),
                Some(invalid) => {
                    return Err(InvalidStrategy(invalid.text).err(invalid.loc));
                }
                None => {
                    return Err(UnexpectedEOF
                        .message(format!("Expected strategy name"), lexer.current_loc()));
                }
            };

            let rule = self
                .rules
                .get(&name)
                .map(|rule| {
                    if reverse {
                        rule.reverse()
                    } else {
                        rule.clone()
                    }
                })
                .ok_or(RuleDoesNotExist(name).err(lexer.current_loc()))?;

            let res = self
                .do_apply(&rule, strategy, n)
                .runtime(ApplyError, lexer.current_loc())?;
            if let Some(res) = res {
                Ok(StepResult::new(
                    if self.verbosity == Verbosity::Silent {
                        vec![]
                    } else {
                        vec![res.0]
                    },
                    res.1,
                    res.2,
                ))
            } else {
                Ok(StepResult::empty())
            }
        } else {
            return Err(NoShape.err(lexer.current_loc()));
        }
    }

    fn cmd_def_shape(
        &mut self,
        mut lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        // Shape definition
        //lexer.catchup();
        let shape = Expr::parse(&mut lexer).runtime(ParseError, lexer.current_loc())?;
        if lexer
            .next_if(|tok| tok.kind == TokenKind::OpenBrace)
            .is_some()
            || lexer.next_if(|tok| tok.kind == TokenKind::Eof).is_some()
        {
            self.shape_stack.push(shape);
            if self.verbosity == Verbosity::Silent {
                Ok(StepResult::empty())
            } else {
                Ok(StepResult::with_results(vec![vec![format!(
                    "{}",
                    self.shape_stack.last().unwrap()
                )]]))
            }
        } else {
            let invalid = lexer.next().unwrap();
            return Err(UnexpectedToken.message(
                format!("Expected open brace or EOL, found {}", invalid),
                invalid.loc,
            ));
        }
    }

    fn cmd_pwd(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let path = env::current_dir()
            .map_err(|e| {
                anyhow::anyhow!(
                    "Cannot get current directory: {}",
                    e.to_string().red().bold()
                )
            })
            .runtime(FileError, lexer.current_loc())?;
        Ok(StepResult::with_results(vec![vec![path
            .to_str()
            .unwrap()
            .to_string()]]))
    }

    fn cmd_cd(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let (path, loc) = match lexer.next() {
            Some(t) => match t {
                lexer::Token {
                    kind: TokenKind::Ident,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::String,
                    text,
                    loc,
                    ..
                } => (text, loc),
                lexer::Token {
                    kind: TokenKind::Path,
                    text,
                    loc,
                    ..
                } => (text, loc),
                invalid => {
                    return Err(
                        InvalidCommand(format!("{} ({:?})", invalid.text, invalid.kind))
                            .message(format!("Expected path."), invalid.loc),
                    )
                }
            },
            None => {
                return Err(UnexpectedEOF.message(format!("Expected path"), lexer.current_loc()))
            }
        };
        let path = PathBuf::from(&path);
        if path.exists() && path.is_dir() {
            env::set_current_dir(&path)
                .map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot change directory: {}, {}",
                        path.to_str().unwrap(),
                        e.to_string().red().bold()
                    )
                })
                .runtime(FileError, loc)?;
            Ok(StepResult::with_results(vec![vec![format!(
                "Changed directory to {}",
                path.to_str().unwrap()
            )]]))
        } else {
            Err(FileError.message(
                format!("{} is not a directory", path.to_str().unwrap().red().bold(),),
                loc,
            ))
        }
    }

    fn cmd_ls(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        lexer.catchup();
        let path = env::current_dir()
            .map_err(|e| {
                anyhow::anyhow!(
                    "Cannot get current directory: {}",
                    e.to_string().red().bold()
                )
            })
            .runtime(FileError, lexer.current_loc())?;
        let mut entries = fs::read_dir(&path)
            .map_err(|e| {
                anyhow::anyhow!(
                    "Cannot read directory: {}, {}",
                    path.to_str().unwrap(),
                    e.to_string().red().bold()
                )
            })
            .runtime(FileError, lexer.current_loc())?;
        let mut files = vec![];
        let mut dirs = vec![format!("{}{}", "..", "/".cyan())];
        while let Some(entry) = entries.next() {
            let entry = entry
                .map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot read directory: {}, {}",
                        path.to_str().unwrap(),
                        e.to_string().red().bold()
                    )
                })
                .runtime(FileError, lexer.current_loc())?;
            let path = entry.path();
            if path.is_file() {
                files.push(path.file_name().unwrap().to_str().unwrap().to_string());
            } else if path.is_dir() {
                dirs.push(format!(
                    "{}{}",
                    path.file_name().unwrap().to_str().unwrap(),
                    "/".cyan()
                ));
            }
        }
        dirs.extend(files.drain(..));
        Ok(StepResult::with_results(vec![dirs]))
    }

    pub fn step(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult, RuntimeError> {
        use TokenKind::*;

        let tok = lexer.peek().clone();
        match tok.kind {
            Command(CommandKind::Help) => self.help(lexer),
            TokenKind::Command(CommandKind::Quit) => {
                lexer.catchup();
                self.quit = true;
                return Ok(StepResult::empty());
            }
            Command(CommandKind::Clear) => {
                lexer.catchup();
                return Ok(StepResult::clear());
            }
            Command(CommandKind::Undo) => self.cmd_undo(lexer),
            Command(CommandKind::Redo) => self.cmd_redo(lexer),
            Command(CommandKind::Save) => self.cmd_save(lexer),
            Command(CommandKind::Use) => self.cmd_use(lexer),
            Command(CommandKind::Run) => self.cmd_run(lexer),
            Command(CommandKind::Ls) => self.cmd_ls(lexer),
            Command(CommandKind::Pwd) => self.cmd_pwd(lexer),
            Command(CommandKind::Cd) => self.cmd_cd(lexer),
            CloseBrace | Command(CommandKind::Done) => self.cmd_done(lexer),
            DoubleDot => self.cmd_all_rules(lexer),
            DoubleColon => self.cmd_anon_rule(lexer),
            Ident => {
                let name = tok.text.clone();
                let next = lexer.peek_next().clone();

                match next.kind {
                    DoubleColon => self.cmd_def_rule(lexer, name),
                    Bar => self.cmd_apply_named_rule(lexer, name),
                    _ => self.cmd_def_shape(lexer),
                }
            }
            TokenKind::Comment => {
                lexer.catchup();
                Ok(StepResult::empty())
            }
            TokenKind::Eof => {
                lexer.catchup();
                return Ok(StepResult::empty());
            }
            _invalid => {
                return Err(InvalidCommand(format!("{}", tok.text.red().bold()))
                    .message("expected command".into(), tok.loc))
            }
        }
    }

    fn do_apply(
        &mut self,
        rule: &Rule,
        strategy: StrategyKind,
        n: String,
    ) -> Result<Option<(Vec<String>, bool, bool)>> {
        enum StrategyResult {
            Apply(Expr),
            Check(ApplyCheck),
        }
        use StrategyResult::*;
        let apply = match strategy {
            StrategyKind::ApplyAll => {
                Apply(rule.apply(&self.shape_stack.last_mut().unwrap(), &mut ApplyAll))
            }
            StrategyKind::ApplyFirst => {
                Apply(rule.apply(&self.shape_stack.last_mut().unwrap(), &mut ApplyFirst))
            }
            StrategyKind::ApplyDeep => {
                Apply(rule.apply(&self.shape_stack.last_mut().unwrap(), &mut ApplyDeep))
            }
            StrategyKind::ApplyNth => Apply(rule.apply(
                &self.shape_stack.last_mut().unwrap(),
                &mut ApplyNth::new(n.parse()?),
            )),
            StrategyKind::Check => {
                let mut check = ApplyCheck::new();
                rule.apply(&self.shape_stack.last().unwrap(), &mut check);
                Check(check)
            }
            #[allow(unreachable_patterns)]
            _ => return Err(InvalidStrategy(n).into()),
        };
        let mut res = vec![];
        let mut indent_each = false;
        if let Apply(apply) = apply {
            self.apply_history.push(std::mem::replace(
                self.shape_stack.last_mut().unwrap(),
                apply,
            ));
            res.push(format!("{}", self.shape_stack.last().unwrap()));
        } else if let Check(mut check) = apply {
            res.extend(
                check
                    .matches()
                    .unwrap()
                    .drain(..)
                    .map(|(from, to)| format!("{} -> {}", from, to)),
            );
            indent_each = true;
        }

        Ok(Some((res, indent_each, false)))
    }
}
