use std::{
    env,
    fs::{self, OpenOptions},
    io::{BufWriter, Write},
    path::PathBuf,
};

use crate::{
    err, err_hl,
    error::{Error, ErrorKind, IntoError, Result, RuntimeError::*, WithMessage},
    expr::{Expr, Repeat},
    lexer::{self, CommandKind, Lexer, Loc, StrategyKind, Token, TokenKind},
    matching::{collect_sub_constexprs, collect_subexprs, pattern_match},
    parse::{self, parse_eval, RuleDefResult, SaveType},
    rule::{ApplyAll, ApplyCheck, ApplyDeep, ApplyFirst, ApplyNth, Rule, Strategy},
};
use crossterm::{
    event::Event,
    style::{Color, ContentStyle, Stylize},
    terminal::disable_raw_mode,
};
use linked_hash_map::LinkedHashMap;

pub(crate) struct Runtime {
    rules: LinkedHashMap<String, Rule>,
    apply_history: Vec<Expr>,
    undo_history: Vec<Expr>,
    shape_stack: Vec<Expr>,
    shaping_rule: Option<(String, Expr)>,
    pub(crate) quit: bool,
    pub(crate) verbosity: Verbosity,
    pub(crate) interaction_hook: Option<Box<dyn Fn(Box<dyn Interaction>) -> InteractionResult>>,
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Verbosity {
    Silent,
    Normal,
    #[allow(dead_code)]
    Verbose,
}

#[allow(unused)]
pub(crate) enum InteractionResult {
    String(String),
    Int(isize),
    UInt(usize),
    Float(f64),
    Bool(bool),
    Option(Option<Box<InteractionResult>>),
    Err(Box<dyn std::error::Error>),
}

pub(crate) trait Interaction {
    fn display(&self) -> String;
    /// Returns Some(value) when the interaction is complete, or none to continue
    fn on_event(&mut self, input: Event) -> Option<InteractionResult>;

    fn on_complete(&mut self, _result: &InteractionResult) {}
}

pub(crate) struct SelectOne {
    pub(crate) prompt: String,
    pub(crate) options: Vec<String>,
    pub(crate) selected: usize,
}

impl SelectOne {
    pub(crate) fn new(prompt: String, options: Vec<String>) -> Self {
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

pub(crate) struct StepResult {
    pub(crate) results: Option<Vec<Vec<String>>>,
    pub(crate) cmd_for_each: bool,
    pub(crate) clear: bool,
}

impl StepResult {
    pub(crate) fn new(results: Vec<Vec<String>>, cmd_for_each: bool, clear: bool) -> Self {
        Self {
            results: Some(results),
            cmd_for_each,
            clear,
        }
    }
    pub(crate) fn empty() -> Self {
        Self {
            results: None,
            cmd_for_each: false,
            clear: false,
        }
    }

    pub(crate) fn clear() -> Self {
        Self {
            results: None,
            cmd_for_each: false,
            clear: true,
        }
    }

    pub(crate) fn with_results(results: Vec<Vec<String>>) -> Self {
        Self {
            results: Some(results),
            cmd_for_each: false,
            clear: false,
        }
    }
}

enum StrategyResult {
    Apply(Expr),
    Check(ApplyCheck),
}

impl Runtime {
    pub(crate) fn new() -> Self {
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

    pub(crate) fn show_shape(&self) -> Option<String> {
        self.shape_stack.last().map(|e| e.to_string())
    }

    fn write_subexpr_highlighted(
        expr: &Expr,
        subexprs: &Vec<&Expr>,
        idx: usize,
        style: ContentStyle,
        parent_highlight: bool,
        loc: Loc,
        writer: &mut dyn std::fmt::Write,
    ) -> Result<()> {
        let subexpr = subexprs.get(idx).map(|x| Ok(x)).unwrap_or(
            err!(Runtime SubExprNotFound(idx), loc.clone())
                .with_message(&format!("Subexpr {} does not exist", idx)),
        )?;
        let highlight = pattern_match(expr, subexpr).is_some() || parent_highlight;
        match expr {
            Expr::List(elements, repeat) => {
                if highlight {
                    write!(writer, "{}", style.apply("(")).inherit(loc.clone())?;
                    for (i, element) in elements.iter().enumerate() {
                        Self::write_subexpr_highlighted(
                            element,
                            subexprs,
                            idx,
                            style,
                            highlight,
                            loc.clone(),
                            writer,
                        )?;
                        if i + 1 < elements.len() {
                            write!(writer, "{}", style.apply(", ")).inherit(loc.clone())?;
                        }
                    }
                    if let Repeat::ZeroOrMore(sep) = repeat {
                        write!(
                            writer,
                            "{}{}",
                            sep.clone().map(|x| x.to_string()).unwrap_or("".to_owned()),
                            "..",
                        )
                        .inherit(loc.clone())?;
                    }
                    write!(writer, "{}", style.apply(")")).inherit(loc.clone())?;
                    Ok(())
                } else {
                    write!(writer, "{}", "(").inherit(loc.clone())?;
                    for (i, element) in elements.iter().enumerate() {
                        Self::write_subexpr_highlighted(
                            element,
                            subexprs,
                            idx,
                            style,
                            highlight,
                            loc.clone(),
                            writer,
                        )?;
                        if i + 1 < elements.len() {
                            write!(writer, "{}", ", ").inherit(loc.clone())?;
                        }
                    }
                    if let Repeat::ZeroOrMore(sep) = repeat {
                        write!(
                            writer,
                            "{}{}",
                            sep.clone().map(|x| x.to_string()).unwrap_or("".to_owned()),
                            "..",
                        )
                        .inherit(loc.clone())?;
                    }
                    write!(writer, "{}", ")").inherit(loc.clone())?;
                    Ok(())
                }
            }
            Expr::Fun(head, body) => {
                if highlight {
                    Self::write_subexpr_highlighted(
                        head,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    Self::write_subexpr_highlighted(
                        body,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                } else {
                    Self::write_subexpr_highlighted(
                        head,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    Self::write_subexpr_highlighted(
                        body,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                }
                Ok(())
            }
            Expr::Op(op, lhs, rhs) => {
                if highlight {
                    write!(writer, "{}", style.apply("(")).inherit(loc.clone())?;
                    Self::write_subexpr_highlighted(
                        lhs,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    write!(writer, " {} ", style.apply(op)).inherit(loc.clone())?;
                    Self::write_subexpr_highlighted(
                        rhs,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    write!(writer, "{}", style.apply(")")).inherit(loc.clone())?;
                    Ok(())
                } else {
                    write!(writer, "(").inherit(loc.clone())?;
                    Self::write_subexpr_highlighted(
                        lhs,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    write!(writer, " {} ", op).inherit(loc.clone())?;
                    Self::write_subexpr_highlighted(
                        rhs,
                        subexprs,
                        idx,
                        style,
                        highlight,
                        loc.clone(),
                        writer,
                    )?;
                    write!(writer, ")").inherit(loc.clone())?;
                    Ok(())
                }
            }
            Expr::Sym(sym) => {
                if highlight {
                    write!(writer, "{}", style.apply(sym)).inherit(loc)?;
                } else {
                    write!(writer, "{}", sym).inherit(loc)?;
                }
                Ok(())
            }
            Expr::Var(var, _) => {
                if highlight {
                    write!(writer, "{}", style.apply(var)).inherit(loc)?;
                } else {
                    write!(writer, "{}", var).inherit(loc)?;
                }
                Ok(())
            }
            Expr::Num(num) => {
                if highlight {
                    write!(writer, "{}", style.apply(num)).inherit(loc)?;
                } else {
                    write!(writer, "{}", num).inherit(loc)?;
                }
                Ok(())
            }
            Expr::Str(str) => {
                if highlight {
                    write!(writer, "\"{}\"", style.apply(str)).inherit(loc)?;
                } else {
                    write!(writer, "\"{}\"", str).inherit(loc)?;
                }
                Ok(())
            }
        }
    }

    fn cmd_use(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        let (file_name, loc) = parse::parse_use(lexer)?;
        let mut path = PathBuf::from(&file_name);
        if let Some(file) = &lexer.file_path {
            path = PathBuf::from(file).parent().unwrap().join(path);
        }
        if !path.exists() {
            return err!(Runtime IOError, loc).with_message(&*format!(
                "{} {}",
                path.to_str().unwrap(),
                "does not exist".red().bold()
            ));
        }
        if path.is_dir() {
            let dir_name = path.file_name().unwrap().to_str().unwrap();
            let new_path = path.join(dir_name).with_extension("noq");
            if new_path.exists() {
                path = new_path;
            } else {
                return err!(Runtime IOError, loc).with_message(&*format!(
                    "{} {}",
                    path.to_str().unwrap(),
                    "is a directory and does not contain a root noq file"
                        .red()
                        .bold()
                ));
            }
        }

        let contents = fs::read_to_string(&path)
            .inherit(loc.clone())
            .with_prefix(&*format!("Could not read {:?}:", &path))?;

        let mut lexer = lexer::Lexer::new(contents.chars().peekable()).with_file(file_name, path);
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

    fn help(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        lexer.catchup();
        let pat = |s: &'static str| s.dark_magenta().bold();
        let cmd = |s: &'static str| s.dark_blue();
        return Ok(StepResult::with_results(vec![vec![format!("\
         Commands:
            {shape}  -------------------- define a shape
            {rule_name} :: {rule}  ------ define a rule
            {rule_name} :: {shape} {{  --- define a rule via shaping (open paren is optional on command line)
            {rule_name} | {strategy}  --- apply a rule to the shape
            :: {rule}   | {strategy}  --- apply an anonymous rule to the shape
            }}  -------------------------- finish the shape
            {done}  ----------------------- finish the shape
            {undo}  ----------------------- undo the last apply
            {redo}  ----------------------- redo the last undo
            {run} {path}  ----------------- run a file
            {use} {path}  ----------------- use rules from a file
            {ls}  ------------------------- list files in current directory
            {cd} {path}  ------------------ change directory
            {pwd}  ------------------------ print current directory
            {clear}  ---------------------- clear the screen
            {quit}  ----------------------- quit the repl
            {help}  ----------------------- print this help message

         Strategies:
            Apply Nth (number, 0-n)      [Example: square | 2 ]
            Apply All (all)              [Example: square | all ]
            Apply Deep (deep)            [Example: square | deep ]
            Check (check)                [Example: square | check ]
                    ",
            strategy = "<strategy>".yellow(),
            shape = pat("<shape>"),
            rule_name = pat("<rule_name>"),
            rule = pat("<rule>"),
            path = pat("<path>"),
            done = cmd("done"),
            undo = cmd("undo"),
            redo = cmd("redo"),
            run = cmd("run"),
            use = cmd("use"),
            ls = cmd("ls"),
            cd = cmd("cd"),
            pwd = cmd("pwd"),
            clear = cmd("clear"),
            quit = cmd("quit"),
            help = cmd("help"),
        )]]));
    }

    fn cmd_undo(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
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
            err!(Runtime NothingToUndo, lexer.current_loc())
        }
    }

    fn cmd_redo(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
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
            err!(Runtime NothingToRedo, lexer.current_loc())
        }
    }

    fn cmd_save(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        let (save_type, file_name, loc) = parse::parse_save(lexer)?;

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
                            return err!(Runtime IOError, loc).with_message(&format!(
                                "File {} already exists",
                                path.to_str().unwrap().red()
                            ));
                        }
                    }
                    _ => unreachable!(),
                }
            }
        } else if !path.exists() {
            fs::File::create(&path)
                .inherit(loc.clone())
                .with_prefix(&format!("Cannot create file {}: ", path.to_str().unwrap(),))?;
            opt.write(true).read(true).truncate(true);
        } else {
            return err!(Runtime IOError, loc).with_message(&format!(
                "{} is a directory",
                path.to_str().unwrap().red().bold(),
            ));
        };

        let file = opt
            .open(&path)
            .inherit(loc.clone())
            .with_prefix(&format!("Cannot open {}:", file_name))?;

        let mut writer = BufWriter::new(file);

        let res = match save_type {
            SaveType::All => {
                for (name, rule) in self.rules.iter() {
                    writer
                        .write_fmt(format_args!("\n{} :: {}", name, rule))
                        .inherit(loc.clone())?;
                    writer.flush().inherit(loc.clone())?;
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
                        .write_fmt(format_args!("\n{} :: {}", name, rule))
                        .inherit(loc.clone())?;
                    writer.flush().inherit(loc.clone())?;
                }
                format!(
                    "Saved {} rules to {}",
                    self.rules.len(),
                    path.to_str().unwrap()
                )
            }
        };
        writer.flush().inherit(loc)?;
        Ok(StepResult::with_results(vec![vec![res]]))
    }

    fn cmd_run(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        let (file_name, loc) = parse::parse_run(lexer)?;
        let path = PathBuf::from(&file_name);
        if !path.exists() {
            return err!(Runtime IOError, loc).with_message(&format!(
                "{} does not exist",
                err_hl!(path.to_str().unwrap()),
            ));
        }
        if path.is_dir() {
            return err!(Runtime IOError, loc).with_message(&format!(
                "{} is a directory",
                path.to_str().unwrap().red().bold(),
            ));
        }
        let contents = fs::read_to_string(&path)
            .inherit(loc)
            .with_prefix(&format!("Cannot open {}:", err_hl!(file_name)))?;
        let mut lexer = lexer::Lexer::new(contents.chars().peekable()).with_file(
            path.file_name().unwrap().to_str().unwrap().to_string(),
            path,
        );
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

    fn cmd_done(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
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
            return err!(Runtime NoShape, lexer.current_loc());
        }
    }

    fn cmd_anon_rule(
        &mut self,
        mut lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult> {
        let loc = lexer.current_loc();
        let (rule, strategy, reversed, n) = parse::parse_anon_rule(&mut lexer)?;
        let res = self.do_apply(
            &if reversed { rule.reverse() } else { rule },
            strategy,
            n,
            loc,
        )?;
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
    ) -> Result<StepResult> {
        let loc = lexer.current_loc();
        let (reverse, strategy, n) = parse::parse_all_rules(lexer)?;
        let mut r = vec![];
        let mut no_matches = vec![];
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
                if let Some(sub_result) =
                    self.do_apply(&rule, strategy.clone(), n.clone(), loc.clone())?
                {
                    let mut new = vec![];
                    if sub_result.0.is_empty() {
                        no_matches.push(name.to_owned());
                    } else {
                        new.push(format!("{}:", name.to_owned().green()));
                        new.extend(
                            sub_result
                                .0
                                .iter()
                                .enumerate()
                                .map(|(idx, s)| format!("{:<2}> {}", idx, s)),
                        );
                        r.push(new);
                    }
                }
                Ok(())
            })
            .collect::<Result<_>>()?;

        if !no_matches.is_empty() {
            r.push(vec![
                format!("{}:", no_matches.join(", ")),
                format!("-> {}", "no matches".red()),
            ]);
        }

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
        lexer: &mut Lexer<impl Iterator<Item = char>>,
        name: String,
    ) -> Result<StepResult> {
        use RuleDefResult::*;
        match parse::parse_def_rule(lexer)? {
            DefViaShape(expr) => {
                self.shaping_rule = Some((name.clone(), expr));
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
            Def(rule) => {
                self.rules.insert(name.clone(), rule);
                Ok(StepResult::with_results(vec![vec![format!(
                    "Rule {} defined",
                    name.yellow()
                )]]))
            }
        }
    }

    fn cmd_apply_named_rule(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
        name: &Token,
    ) -> Result<StepResult> {
        if self.shape_stack.last().is_none() {
            return err!(Runtime NoShape, lexer.current_loc());
        }
        // Apply rule
        if &*name.text == "eval" {
            let (strategy, n) = parse_eval(lexer)?;
            let res = self.do_eval(strategy, n, name.loc.clone())?;
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
            let (reverse, strategy, n) = parse::parse_apply_named_rule(lexer)?;

            let rule = self
                .rules
                .get(&name.text)
                .map(|rule| {
                    Ok(if reverse {
                        rule.reverse()
                    } else {
                        rule.clone()
                    })
                })
                .unwrap_or(err!(Runtime RuleDoesNotExist(name.text.clone()), name.loc.clone()))?;

            let res = self.do_apply(&rule, strategy, n, name.loc.clone())?;
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
    }

    fn cmd_def_shape(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult> {
        // Shape definition
        //let loc = lexer.peek().loc.clone();
        let shape = parse::parse_def_shape(lexer)?;
        if self.shape_stack.is_empty() {
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
            return err!(Runtime AlreadyShaping, Loc::default());
        }
    }

    fn cmd_pwd(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        let path = parse::parse_pwd(lexer)?;
        Ok(StepResult::with_results(vec![vec![path
            .to_str()
            .unwrap()
            .to_string()]]))
    }

    fn cmd_cd(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        let (path, loc) = parse::parse_cd(lexer)?;
        let path = PathBuf::from(&path);
        if path.exists() && path.is_dir() {
            env::set_current_dir(&path)
                .inherit(loc)
                .with_prefix(&format!(
                    "Cannot change directory to {}: ",
                    path.to_str().unwrap()
                ))?;
            Ok(StepResult::with_results(vec![vec![format!(
                "Changed directory to {}",
                path.to_str().unwrap()
            )]]))
        } else {
            return err!(Runtime IOError, loc).with_message(&format!(
                "{} is not a directory",
                err_hl!(path.to_str().unwrap()),
            ));
        }
    }

    fn cmd_ls(&mut self, lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<StepResult> {
        lexer.catchup();
        let path = env::current_dir()
            .inherit(lexer.current_loc())
            .with_prefix(&format!("Cannot get current directory: "))?;
        let mut entries = fs::read_dir(&path)
            .inherit(lexer.current_loc())
            .with_prefix(&format!(
                "Cannot read directory {}: ",
                path.to_str().unwrap()
            ))?;

        let mut files = vec![];
        let mut dirs = vec![format!("{}{}", "..", "/".cyan())];
        while let Some(Ok(entry)) = entries.next() {
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

    pub(crate) fn step(
        &mut self,
        lexer: &mut Lexer<impl Iterator<Item = char>>,
    ) -> Result<StepResult> {
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
                let next = lexer.peek_next();

                match next.kind {
                    DoubleColon => self.cmd_def_rule(lexer, tok.text),
                    Bar => self.cmd_apply_named_rule(lexer, &tok),
                    _ => self.cmd_def_shape(lexer),
                }
            }
            Eval => {
                let next = lexer.peek_next();

                match next.kind {
                    Bar => self.cmd_apply_named_rule(lexer, &tok),
                    _ => err!(Runtime InvalidCommand(err_hl!(tok.text)), tok.loc)
                        .with_message("expected '|' after eval".into()),
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
            TokenKind::UnclosedStr => {
                lexer.catchup();

                return err!(Runtime InvalidCommand(err_hl!(tok.text)), tok.loc)
                    .with_message("Unclosed string encountered".into());
            }
            _maybe_expr => self.cmd_def_shape(lexer),
        }
    }

    fn do_eval(
        &mut self,
        strategy: StrategyKind,
        n: String,
        loc: Loc,
    ) -> Result<Option<(Vec<String>, bool, bool)>> {
        use StrategyResult::*;
        let apply = match strategy {
            StrategyKind::ApplyAll => {
                Apply(self.shape_stack.last_mut().unwrap().eval(&mut ApplyAll))
            }
            StrategyKind::ApplyFirst => {
                Apply(self.shape_stack.last_mut().unwrap().eval(&mut ApplyFirst))
            }
            StrategyKind::ApplyDeep => {
                Apply(self.shape_stack.last_mut().unwrap().eval(&mut ApplyDeep))
            }
            StrategyKind::ApplyNth => Apply(self.shape_stack.last_mut().unwrap().eval(
                &mut ApplyNth::new(n.parse().runtime(EvalError, loc.clone())?),
            )),
            StrategyKind::Check => {
                let mut check = ApplyCheck::new();
                self.shape_stack.last().unwrap().eval(&mut check);
                Check(check)
            }
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
            let matches = check.matches().unwrap();
            let mut matches_str = vec![];
            for (match_idx, (from, to)) in matches.iter().enumerate() {
                let from_subexprs = collect_sub_constexprs(&self.shape_stack.last().unwrap());
                let Some(from_idx) = from_subexprs
                    .iter()
                    .enumerate()
                    .find(|(_, expr)| **expr == from) else {
                    continue;
                };
                let mut from_str = String::new();
                Self::write_subexpr_highlighted(
                    self.shape_stack.last().unwrap(),
                    &from_subexprs,
                    from_idx.0,
                    ContentStyle::default().with(Color::Red),
                    false,
                    loc.clone(),
                    &mut from_str,
                )?;

                let applied = self
                    .shape_stack
                    .last()
                    .unwrap()
                    .eval(&mut ApplyNth::new(match_idx));

                let to_subexprs = collect_subexprs(to, &applied);
                let Some(to_idx) = to_subexprs
                .iter()
                .enumerate()
                .find(|(_, expr)| **expr == to) else {
                    continue;
                };
                let mut to_str = String::new();

                Self::write_subexpr_highlighted(
                    &applied,
                    &to_subexprs,
                    to_idx.0,
                    ContentStyle::default().with(Color::Green),
                    false,
                    loc.clone(),
                    &mut to_str,
                )?;

                matches_str.push(format!("{} -> {}", from_str, to_str));
            }
            res.extend(matches_str);

            indent_each = false;
        }

        Ok(Some((res, indent_each, false)))
    }

    fn do_apply(
        &mut self,
        rule: &Rule,
        strategy: StrategyKind,
        n: String,
        loc: Loc,
    ) -> Result<Option<(Vec<String>, bool, bool)>> {
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
                &mut ApplyNth::new(n.parse().runtime(ApplyError, loc.clone())?),
            )),
            StrategyKind::Check => {
                let mut check = ApplyCheck::new();
                rule.apply(&self.shape_stack.last().unwrap(), &mut check);
                Check(check)
            }
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
            let matches = check.matches().unwrap();
            let mut matches_str = vec![];
            for (match_idx, (from, to)) in matches.iter().enumerate() {
                let from_subexprs = collect_subexprs(&rule.head, &self.shape_stack.last().unwrap());
                let Some(from_idx) = from_subexprs
                    .iter()
                    .enumerate()
                    .find(|(_, expr)| **expr == from) else {
                    continue;
                };
                let mut from_str = String::new();
                Self::write_subexpr_highlighted(
                    self.shape_stack.last().unwrap(),
                    &from_subexprs,
                    from_idx.0,
                    ContentStyle::default().with(Color::Red),
                    false,
                    loc.clone(),
                    &mut from_str,
                )?;

                let applied = rule.apply(
                    &self.shape_stack.last().unwrap(),
                    &mut ApplyNth::new(match_idx),
                );
                let to_subexprs = collect_subexprs(to, &applied);
                let Some(to_idx) = to_subexprs
                .iter()
                .enumerate()
                .find(|(_, expr)| **expr == to) else {
                    continue;
                };
                let mut to_str = String::new();

                Self::write_subexpr_highlighted(
                    &applied,
                    &to_subexprs,
                    to_idx.0,
                    ContentStyle::default().with(Color::Green),
                    false,
                    loc.clone(),
                    &mut to_str,
                )?;

                matches_str.push(format!("{} -> {}", from_str, to_str));
            }
            res.extend(matches_str);

            indent_each = true;
        }

        Ok(Some((res, indent_each, false)))
    }
}
