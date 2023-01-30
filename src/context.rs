use std::{
    fs::{self, File},
    iter::Peekable,
    rc::Rc,
};

use crate::{
    expr::{self, Expr},
    lexer::{self, CommandKind, Lexer, StrategyKind, Token, TokenKind},
    rule::{ApplyAll, ApplyDeep, ApplyFirst, Rule},
};
use anyhow::Result;
use crossterm::style::Stylize;
use linked_hash_map::LinkedHashMap;

pub struct Context {
    rules: LinkedHashMap<String, Rule>,
    apply_history: Vec<Expr>,
    undo_history: Vec<Expr>,
    shape: Option<Expr>,
    pub quit: bool,
    pub quiet: bool,
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
    #[error("Unexpected end of file")]
    UnexpectedEOF,
}
use crate::context::Error::*;

impl Context {
    pub fn new() -> Self {
        Self {
            rules: LinkedHashMap::new(),
            apply_history: Vec::new(),
            undo_history: Vec::new(),
            shape: None,
            quiet: false,
            quit: false,
        }
    }

    pub fn rebuild(&self) -> Self {
        Self {
            quit: false,
            rules: self.rules.clone(),
            apply_history: self.apply_history.clone(),
            undo_history: self.undo_history.clone(),
            shape: self.shape.clone(),
            quiet: self.quiet,
        }
    }

    pub fn set_quiet(&mut self, quiet: bool) {
        self.quiet = quiet;
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
    ) -> Result<Option<String>> {
        use TokenKind::*;
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
                    Ok(None)
                } else {
                    Ok(Some(format!("Rule {} defined", name.yellow())))
                }
            }
            Some(lexer::Token {
                kind: Command(CommandKind::Apply),
                ..
            }) => {
                if let Some(shape) = &mut self.shape {
                    let (strategy, strat_name) = match lexer.next() {
                        Some(lexer::Token {
                            kind: TokenKind::Strategy(strategy),
                            text,
                        }) => (strategy, text),
                        Some(invalid) => {
                            return Err(InvalidStrategy(invalid.text).into());
                        }
                        None => {
                            return Err(UnexpectedEOF.into());
                        }
                    };

                    let rule = match lexer.next() {
                        Some(lexer::Token {
                            kind: TokenKind::Command(CommandKind::Rule),
                            ..
                        }) => Rule::parse(&mut lexer)?,
                        Some(lexer::Token {
                            kind: TokenKind::Ident,
                            text: name,
                        }) => self.rules.get(&name).ok_or(RuleDoesNotExist(name))?.clone(),
                        _ => {
                            return Err(ExpectedRuleExprOrAnon.into());
                        }
                    };
                    self.apply_history.push(std::mem::replace(
                        shape,
                        match strategy {
                            StrategyKind::All => rule.apply(&shape, &mut ApplyAll),
                            StrategyKind::First => rule.apply(&shape, &mut ApplyFirst),
                            StrategyKind::Deep => rule.apply(&shape, &mut ApplyDeep),
                            #[allow(unreachable_patterns)]
                            _ => return Err(InvalidStrategy(strat_name).into()),
                        },
                    ));

                    Ok(Some(format!("{}", self.shape.clone().unwrap())))
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
                Ok(Some(format!("{}", self.shape.clone().unwrap())))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Done),
                ..
            }) => {
                if let Some(shape) = &self.shape {
                    let shape = shape.clone();
                    self.shape = None;
                    Ok(Some(format!(
                        "{} {}",
                        shape.to_string().green(),
                        "\u{2714}".green().bold()
                    )))
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
                    Ok(Some(format!("{}", self.shape.clone().unwrap())))
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
                    Ok(Some(format!("{}", self.shape.clone().unwrap())))
                } else {
                    Err(NothingToRedo.into())
                }
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Help),
                ..
            }) => {
                let color = |s: &'static str| s.dark_cyan().bold();
                let underline = |s: &'static str| s.underlined();
                Ok(Some(format!(
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
                )))
            }
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Quit),
                ..
            }) => {
                self.quit = true;
                Ok(None)
            }
            Some(lexer::Token {
                kind: TokenKind::Comment,
                ..
            }) => Ok(None),
            Some(lexer::Token {
                kind: TokenKind::Command(CommandKind::Load),
                text,
            }) => {
                let file = match lexer.next() {
                    Some(t) => match t {
                        lexer::Token {
                            kind: TokenKind::Ident,
                            text,
                        } => text,
                        _ => return Err(InvalidCommand(format!("{}", text.red().bold())).into()),
                    },
                    None => return Err(UnexpectedEOF.into()),
                };
                let contents = fs::read_to_string(&file)
                    .map_err(|e| anyhow::anyhow!("{}: {}", file, e.to_string().red().bold()))?;
                let mut lexer = lexer::Lexer::new(contents.chars().peekable());
                while !lexer.exhausted {
                    self.step(&mut lexer)?;
                }

                Ok(None)
            }
            Some(lexer::Token {
                kind: TokenKind::Eof,
                ..
            }) => Ok(None),
            Some(invalid) => Err(InvalidCommand(format!("{}", invalid.text.red().bold())).into()),
            None => Err(UnexpectedEOF.into()),
        }
    }
}
