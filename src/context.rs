use std::iter::Peekable;

use crate::{
    lexer::{self, Lexer, Token},
    rule::{self, Expr, Rule},
};
use anyhow::Result;
use crossterm::style::Stylize;
use linked_hash_map::LinkedHashMap;

pub struct Context<'a> {
    pub lexer: Peekable<lexer::Lexer<'a>>,
    rules: LinkedHashMap<String, Rule>,
    apply_history: Vec<Expr>,
    undo_history: Vec<Expr>,
    shape: Option<Expr>,
    pub quit: bool,
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Invalid command {0}. Expected one of {}", crate::commands!())]
    InvalidCommand(String),
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

impl<'a> Context<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            rules: LinkedHashMap::new(),
            apply_history: Vec::new(),
            undo_history: Vec::new(),
            shape: None,
            quit: false,
        }
    }

    pub fn rebuild(&self, lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            quit: false,
            rules: self.rules.clone(),
            apply_history: self.apply_history.clone(),
            undo_history: self.undo_history.clone(),
            shape: self.shape.clone(),
        }
    }

    pub fn show_shape(&self) -> Result<String> {
        if let Some(shape) = &self.shape {
            Ok(format!("{}", shape))
        } else {
            Err(NoShape.into())
        }
    }

    pub fn run_cmd(&mut self) -> Result<Option<String>> {
        let mut lexer = &mut self.lexer;
        match lexer.next() {
            Some(lexer::Token {
                kind: lexer::TokenKind::Rule,
                ..
            }) => {
                let Some(Token {
                    kind: lexer::TokenKind::Sym,
                    text: name,
                }) = lexer.next() else {
                    return Err(ExpectedRuleName.into());
                };
                self.rules.insert(name, Rule::parse(&mut lexer)?);
                Ok(None)
            }
            Some(lexer::Token {
                kind: lexer::TokenKind::Apply,
                ..
            }) => {
                if let Some(shape) = &mut self.shape {
                    match lexer.next() {
                        Some(lexer::Token {
                            kind: lexer::TokenKind::Rule,
                            ..
                        }) => {
                            self.apply_history.push(std::mem::replace(
                                shape,
                                rule::Rule::parse(&mut lexer)?.apply_all(&shape)?,
                            ));
                        }
                        Some(lexer::Token {
                            kind: lexer::TokenKind::Sym,
                            text: name,
                        }) => {
                            self.apply_history.push(std::mem::replace(
                                shape,
                                self.rules
                                    .get(&name)
                                    .ok_or(RuleDoesNotExist(name))?
                                    .apply_all(&shape)?,
                            ));
                        }
                        _ => {
                            return Err(ExpectedRuleExprOrAnon.into());
                        }
                    }

                    Ok(Some(format!("{}", self.shape.clone().unwrap())))
                } else {
                    return Err(NoShape.into());
                }
            }
            Some(lexer::Token {
                kind: lexer::TokenKind::Shape,
                ..
            }) => {
                if let Some(_) = lexer.peek() {
                    if self.shape.is_some() {
                        return Err(AlreadyShaping.into());
                    }
                    self.shape = Some(rule::Expr::parse(&mut lexer)?);
                    Ok(Some(format!("{}", self.shape.clone().unwrap())))
                } else {
                    if let Some(shape) = &self.shape {
                        return Ok(Some(format!("{}", shape)));
                    } else {
                        return Err(NoShape.into());
                    }
                }
            }
            Some(lexer::Token {
                kind: lexer::TokenKind::Done,
                ..
            }) => {
                if let Some(shape) = &self.shape {
                    let shape = shape.clone();
                    self.shape = None;
                    Ok(Some(format!(
                        "{} {}",
                        shape.to_string().green(),
                        "\u{2714}\n".green().bold()
                    )))
                } else {
                    Err(NoShape.into())
                }
            }
            Some(lexer::Token {
                kind: lexer::TokenKind::Undo,
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
                kind: lexer::TokenKind::Redo,
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
                kind: lexer::TokenKind::Help,
                ..
            }) => {
                let color = |s: &'a str| s.dark_cyan().bold();
                let underline = |s: &'a str| s.underlined();
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
                kind: lexer::TokenKind::Quit,
                ..
            }) => {
                self.quit = true;
                Ok(None)
            }
            Some(invalid) => Err(InvalidCommand(format!("{}", invalid.text.red().bold())).into()),
            None => Err(UnexpectedEOF.into()),
        }
    }
}
