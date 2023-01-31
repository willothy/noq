use std::{
    env,
    fs::{self, OpenOptions},
    io::{BufWriter, Write},
    path::PathBuf,
};

use crate::{
    expr::{self, Expr},
    lexer::{self, CommandKind, Lexer, StrategyKind, Token, TokenKind},
    rule::{ApplyAll, ApplyCheck, ApplyDeep, ApplyFirst, ApplyNth, Rule, Strategy},
};
use anyhow::Result;
use crossterm::{event::Event, style::Stylize};
use linked_hash_map::LinkedHashMap;

pub struct Runtime {
    rules: LinkedHashMap<String, Rule>,
    apply_history: Vec<Expr>,
    undo_history: Vec<Expr>,
    shape: Option<Expr>,
    pub quit: bool,
    pub quiet: bool,
    pub interaction_hook: Option<Box<dyn Fn(Box<dyn Interaction>) -> InteractionResult>>,
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Invalid command {0}. Expected one of {}", crate::commands!())]
    InvalidCommand(String),
    #[error("Invalid strategy {0}. Expected one of {}", crate::strategies!())]
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
    #[error("Unexpected end of file: {0}")]
    UnexpectedEOF(String),
}
use crate::runtime::Error::*;

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

    fn on_complete(&mut self, result: &InteractionResult) {}
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

    fn on_complete(&mut self, result: &InteractionResult) {}
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
            shape: None,
            quiet: false,
            quit: false,
            interaction_hook: None,
        }
    }

    pub fn show_shape(&self) -> Result<String> {
        if let Some(shape) = &self.shape {
            Ok(format!("{}", shape))
        } else {
            Err(NoShape.into())
        }
    }

    pub fn step(
        &mut self,
        mut lexer: &mut Lexer<impl Iterator<Item = char>>,
        rules_only: bool,
    ) -> Result<StepResult> {
        use TokenKind::*;
        if rules_only {
            loop {
                match lexer.peek().kind {
                    Command(CommandKind::Rule) => break,
                    Eof => {
                        lexer.next();
                        return Ok(StepResult::empty());
                    }
                    _ => {
                        lexer.next();
                    }
                }
            }
            if lexer.exhausted {
                return Ok(StepResult::empty());
            }
        }

        match lexer.next() {
            Some(lexer::Token {
                kind: Command(CommandKind::Rule),
                ..
            }) => {
                let Some(Token {
                    kind: TokenKind::Ident,
                    text: name,
                }) = lexer.next() else {
                    return Err(ExpectedRuleName.into());
                };
                self.rules.insert(name.clone(), Rule::parse(&mut lexer)?);
                if self.quiet {
                    Ok(StepResult::empty())
                } else {
                    Ok(StepResult::with_results(vec![vec![format!(
                        "Rule {} defined",
                        name.yellow()
                    )]]))
                }
            }
            Some(lexer::Token {
                kind: Command(CommandKind::Apply),
                ..
            }) => {
                if self.shape.is_some() {
                    let (strategy, n) = match lexer.next() {
                        Some(lexer::Token {
                            kind: TokenKind::Strategy(strategy),
                            text,
                        }) => (strategy, text),
                        Some(invalid) => {
                            return Err(InvalidStrategy(invalid.text).into());
                        }
                        None => {
                            return Err(UnexpectedEOF(format!("Expected strategy name")).into());
                        }
                    };

                    let reverse = lexer.next_if(|t| t.kind == TokenKind::Reverse).is_some();

                    match lexer.next() {
                        Some(lexer::Token {
                            kind: TokenKind::Strategy(StrategyKind::ApplyAll),
                            ..
                        }) => {
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
                                    let sub_result =
                                        self.do_apply(&rule, strategy.clone(), n.clone())?;
                                    if let Some(sub_result) = sub_result {
                                        let mut new = vec![];
                                        if !sub_result.0.is_empty() {
                                            new.push(format!("{}:", name.to_owned().green()));
                                            new.extend(
                                                sub_result.0.iter().map(|s| format!("-> {}", s)),
                                            );
                                        } else {
                                            new.push(format!("{}:", name));
                                            new.push(format!("-> {}", "no matches".red()))
                                        }
                                        r.push(new);
                                    }
                                    Ok(())
                                })
                                .collect::<Result<_>>()?;

                            Ok(StepResult::with_results(r))
                        }
                        Some(lexer::Token {
                            kind: TokenKind::Command(CommandKind::Rule),
                            ..
                        }) => {
                            let rule = Rule::parse(&mut lexer).map(|rule| {
                                if reverse {
                                    rule.reverse()
                                } else {
                                    rule
                                }
                            })?;
                            let res = self.do_apply(&rule, strategy, n)?;
                            if let Some(res) = res {
                                Ok(StepResult::new(vec![res.0], res.1, res.2))
                            } else {
                                Ok(StepResult::empty())
                            }
                        }
                        Some(lexer::Token {
                            kind: TokenKind::Ident,
                            text: name,
                        }) => {
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
                                .ok_or(RuleDoesNotExist(name))?;

                            let res = self.do_apply(&rule, strategy, n)?;
                            if let Some(res) = res {
                                Ok(StepResult::new(vec![res.0], res.1, res.2))
                            } else {
                                Ok(StepResult::empty())
                            }
                        }
                        _ => {
                            return Err(ExpectedRuleExprOrAnon.into());
                        }
                    }
                } else {
                    return Err(NoShape.into());
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Shape),
                ..
            }) => {
                if self.shape.is_some() {
                    return Err(AlreadyShaping.into());
                }
                self.shape = Some(expr::Expr::parse(lexer)?);
                Ok(StepResult::with_results(vec![vec![format!(
                    "{}",
                    self.shape.clone().unwrap()
                )]]))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Done),
                ..
            }) => {
                if let Some(shape) = &self.shape {
                    let shape = shape.clone();
                    self.shape = None;
                    Ok(StepResult::with_results(vec![vec![format!(
                        "{} {}",
                        shape.to_string().green(),
                        "\u{2714}".green().bold()
                    )]]))
                } else {
                    Err(NoShape.into())
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Undo),
                ..
            }) => {
                if let Some(shape) = self.apply_history.pop() {
                    self.undo_history
                        .push(std::mem::replace(&mut self.shape, Some(shape)).unwrap());
                    Ok(StepResult::with_results(vec![vec![format!(
                        "{}",
                        self.shape.clone().unwrap()
                    )]]))
                } else {
                    Err(NothingToUndo.into())
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Redo),
                ..
            }) => {
                if let Some(shape) = self.undo_history.pop() {
                    self.apply_history
                        .push(std::mem::replace(&mut self.shape, Some(shape)).unwrap());
                    Ok(StepResult::with_results(vec![vec![format!(
                        "{}",
                        self.shape.clone().unwrap()
                    )]]))
                } else {
                    Err(NothingToRedo.into())
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Load),
                text,
            }) => {
                let file_name = match lexer.next() {
                    Some(t) => match t {
                        lexer::Token {
                            kind: TokenKind::Ident,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::String,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::Path,
                            text,
                        } => text,
                        _ => return Err(InvalidCommand(format!("{}", text.red().bold())).into()),
                    },
                    None => return Err(UnexpectedEOF(format!("Expected file name")).into()),
                };
                let path = PathBuf::from(&file_name);
                if !path.exists() {
                    return Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "does not exist".red().bold()
                    ))
                    .into());
                }
                if path.is_dir() {
                    return Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "is a directory".red().bold()
                    ))
                    .into());
                }
                let contents = fs::read_to_string(&path).map_err(|e| {
                    anyhow::anyhow!("{}: {}", path.to_str().unwrap(), e.to_string().red().bold())
                })?;
                let mut lexer = lexer::Lexer::new(contents.chars().peekable());
                let mut res = vec![];
                while !lexer.exhausted {
                    res.push(self.step(&mut lexer, true)?);
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
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Run),
                text,
            }) => {
                let file_name = match lexer.next() {
                    Some(t) => match t {
                        lexer::Token {
                            kind: TokenKind::Ident,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::String,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::Path,
                            text,
                        } => text,
                        _ => return Err(InvalidCommand(format!("{}", text.red().bold())).into()),
                    },
                    None => return Err(UnexpectedEOF(format!("Expected file name")).into()),
                };
                let path = PathBuf::from(&file_name);
                if !path.exists() {
                    return Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "does not exist".red().bold()
                    ))
                    .into());
                }
                if path.is_dir() {
                    return Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "is a directory".red().bold()
                    ))
                    .into());
                }
                let contents = fs::read_to_string(&path).map_err(|e| {
                    anyhow::anyhow!("{}: {}", path.to_str().unwrap(), e.to_string().red().bold())
                })?;
                let mut lexer = lexer::Lexer::new(contents.chars().peekable());
                let mut res = vec![];
                while !lexer.exhausted {
                    res.push(self.step(&mut lexer, false)?);
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
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Save),
                ..
            }) => {
                enum SaveType {
                    All,
                    Commands,
                    Rules,
                }
                let save_type = match lexer.next() {
                    Some(tok) => match tok.kind {
                        TokenKind::Rules => SaveType::Rules,
                        TokenKind::Commands => SaveType::Commands,
                        TokenKind::Strategy(StrategyKind::ApplyAll) => SaveType::All,
                        _ => {
                            return Err(InvalidCommand(format!(
                                "{}, Expected save type (all, commands, or rules)",
                                tok.text.red().bold()
                            ))
                            .into())
                        }
                    },
                    None => {
                        return Err(UnexpectedEOF(format!(
                            "Expected save type (all, commands, rules) and file name"
                        ))
                        .into())
                    }
                };
                let file_name = match lexer.next() {
                    Some(t) => match t {
                        lexer::Token {
                            kind: TokenKind::Ident,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::String,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::Path,
                            text,
                        } => text,
                        invalid => {
                            return Err(InvalidCommand(format!(
                                "{}, expected file name.",
                                invalid.text.red().bold()
                            ))
                            .into())
                        }
                    },
                    None => return Err(UnexpectedEOF(format!("Expected file name")).into()),
                };
                let path = PathBuf::from(&file_name);
                let mut opt = OpenOptions::new();
                if path.exists() && path.is_file() {
                    if let Some(hook) = self.interaction_hook.as_mut() {
                        let interaction = SelectOne::new(
                            format!("File {} already exists, overwrite?", path.to_str().unwrap()),
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
                                    ));
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                } else if !path.exists() {
                    fs::File::create(&path).map_err(|e| {
                        anyhow::anyhow!(
                            "Cannot create file: {}, {}",
                            path.to_str().unwrap(),
                            e.to_string().red().bold()
                        )
                    })?;
                    opt.write(true).read(true).truncate(true);
                } else {
                    return Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "is a directory".red().bold()
                    ))
                    .into());
                };

                let file = opt.open(&path).map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot open file: {}, {}",
                        path.to_str().unwrap(),
                        e.to_string().red().bold()
                    )
                })?;
                let mut writer = BufWriter::new(file);

                let res = match save_type {
                    SaveType::All => {
                        for (name, rule) in self.rules.iter() {
                            writer.write_fmt(format_args!("rule {} {}\n", name, rule))?;
                            writer.flush()?;
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
                            writer.write_fmt(format_args!("rule {} {}\n", name, rule))?;
                            writer.flush()?;
                        }
                        format!(
                            "Saved {} rules to {}",
                            self.rules.len(),
                            path.to_str().unwrap()
                        )
                    }
                };
                writer.flush()?;
                Ok(StepResult::with_results(vec![vec![res]]))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Clear),
                ..
            }) => Ok(StepResult::clear()),
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Help),
                ..
            }) => {
                let color = |s: &'static str| s.dark_cyan().bold();
                let underline = |s: &'static str| s.underlined();
                Ok(StepResult::with_results(vec![vec![format!(
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
                )]]))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Cd),
                ..
            }) => {
                let path = match lexer.next() {
                    Some(t) => match t {
                        lexer::Token {
                            kind: TokenKind::Ident,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::String,
                            text,
                        } => text,
                        lexer::Token {
                            kind: TokenKind::Path,
                            text,
                        } => text,
                        invalid => {
                            return Err(
                                InvalidCommand(format!("{:?}, expected path.", invalid)).into()
                            )
                        }
                    },
                    None => return Err(UnexpectedEOF(format!("Expected path")).into()),
                };
                let path = PathBuf::from(&path);
                if path.exists() && path.is_dir() {
                    env::set_current_dir(&path).map_err(|e| {
                        anyhow::anyhow!(
                            "Cannot change directory: {}, {}",
                            path.to_str().unwrap(),
                            e.to_string().red().bold()
                        )
                    })?;
                    Ok(StepResult::with_results(vec![vec![format!(
                        "Changed directory to {}",
                        path.to_str().unwrap()
                    )]]))
                } else {
                    Err(InvalidCommand(format!(
                        "{} {}",
                        path.to_str().unwrap(),
                        "is not a directory".red().bold()
                    ))
                    .into())
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Ls),
                ..
            }) => {
                let path = env::current_dir().map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot get current directory: {}",
                        e.to_string().red().bold()
                    )
                })?;
                let mut entries = fs::read_dir(&path).map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot read directory: {}, {}",
                        path.to_str().unwrap(),
                        e.to_string().red().bold()
                    )
                })?;
                let mut files = vec![];
                let mut dirs = vec![format!("{}{}", "..", "/".cyan())];
                while let Some(entry) = entries.next() {
                    let entry = entry.map_err(|e| {
                        anyhow::anyhow!(
                            "Cannot read directory: {}, {}",
                            path.to_str().unwrap(),
                            e.to_string().red().bold()
                        )
                    })?;
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
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Pwd),
                ..
            }) => {
                let path = env::current_dir().map_err(|e| {
                    anyhow::anyhow!(
                        "Cannot get current directory: {}",
                        e.to_string().red().bold()
                    )
                })?;
                Ok(StepResult::with_results(vec![vec![path
                    .to_str()
                    .unwrap()
                    .to_string()]]))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Quit),
                ..
            }) => {
                self.quit = true;
                Ok(StepResult::empty())
            }
            Some(lexer::Token {
                kind: TokenKind::Comment,
                ..
            }) => Ok(StepResult::empty()),
            Some(lexer::Token {
                kind: TokenKind::Eof,
                ..
            }) => Ok(StepResult::empty()),
            Some(invalid) => Err(InvalidCommand(format!("{}", invalid.text.red().bold())).into()),
            None => Err(UnexpectedEOF(format!("Expected command")).into()),
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
                Apply(rule.apply(&self.shape.as_mut().unwrap(), &mut ApplyAll))
            }
            StrategyKind::ApplyFirst => {
                Apply(rule.apply(&self.shape.as_mut().unwrap(), &mut ApplyFirst))
            }
            StrategyKind::ApplyDeep => {
                Apply(rule.apply(&self.shape.as_mut().unwrap(), &mut ApplyDeep))
            }
            StrategyKind::ApplyNth => Apply(rule.apply(
                &self.shape.as_mut().unwrap(),
                &mut ApplyNth::new(n.parse()?),
            )),
            StrategyKind::Check => {
                let mut check = ApplyCheck::new();
                rule.apply(&self.shape.as_mut().unwrap(), &mut check);
                Check(check)
            }
            #[allow(unreachable_patterns)]
            _ => return Err(InvalidStrategy(n).into()),
        };
        let mut res = vec![];
        let mut indent_each = false;
        if let Apply(apply) = apply {
            self.apply_history
                .push(std::mem::replace(self.shape.as_mut().unwrap(), apply));
            res.push(format!("{}", self.shape.clone().unwrap()));
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
