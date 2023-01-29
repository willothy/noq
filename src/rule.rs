use std::{fmt::Display, iter::Peekable};

use anyhow::{bail, Result};
use thiserror::Error;

use crate::{
    bindings::{pattern_match, substitute_bindings},
    lexer::{Token, TokenKind},
};

#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub head: Expr,
    pub body: Expr,
}

impl Rule {
    pub fn parse(lexer: impl Iterator<Item = Token>) -> Result<Self> {
        let mut lexer = lexer.peekable();
        let head = Expr::parse_peekable(&mut lexer)?;
        lexer.next_if(|t| t.kind == TokenKind::Equals).unwrap();
        let body = Expr::parse_peekable(&mut lexer)?;
        match lexer.peek() {
            Some(t) => match t.kind {
                TokenKind::Semicolon => {
                    lexer.next();
                }
                _ => bail!("Expected semicolon, got {:?}", t),
            },
            None => {}
        };
        Ok(Self { head, body })
    }
}

impl Rule {
    pub fn apply_all(&self, expr: &Expr) -> Result<Expr> {
        Ok(if let Some(bindings) = pattern_match(&self.head, expr) {
            substitute_bindings(&bindings, &self.body)?
        } else {
            use Expr::*;
            match expr {
                Sym(_) | Var(_) => expr.clone(),
                Fun(head, args) => Expr::Fun(
                    box self.apply_all(head)?,
                    args.iter()
                        .map(|arg| self.apply_all(arg))
                        .collect::<Result<_, _>>()?,
                ),
            }
        })
    }
}

impl TryFrom<&str> for Rule {
    type Error = anyhow::Error;
    fn try_from(s: &str) -> Result<Self> {
        Rule::parse(crate::lexer::Lexer::from_iter(s.chars().peekable()))
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.head, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Sym(String),
    Var(String),
    Fun(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Error)]
#[error("Parse error: {0}")]
pub struct ParseError(String);

impl Expr {
    fn var_or_sym(name: &str) -> Result<Expr> {
        if name.is_empty() {
            bail!("Empty symbol name")
        }
        Ok(name
            .chars()
            .nth(0)
            .filter(|c| c.is_uppercase())
            .map(|_| Expr::Var(name.to_owned()))
            .unwrap_or(Expr::Sym(name.to_owned())))
    }

    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Self> {
        if let Some(name) = lexer.next() {
            if let Token {
                kind: TokenKind::Sym,
                ..
            } = &name
            {
                if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                    let mut args = Vec::new();
                    if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_some() {
                        return Ok(Expr::Fun(box Self::var_or_sym(&name.text)?, args));
                    }
                    args.push(Self::parse_peekable(lexer)?);
                    while lexer.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                        args.push(Self::parse_peekable(lexer)?);
                    }
                    if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                        return Err(ParseError("Expected ')'".to_string()).into());
                    }
                    Ok(Expr::Fun(box Self::var_or_sym(&name.text)?, args))
                } else {
                    Self::var_or_sym(&name.text)
                }
            } else {
                return Err(ParseError(format!(
                    "Expected symbol, found {:?}",
                    lexer.peek().unwrap()
                ))
                .into());
            }
        } else {
            return Err(ParseError("Expected symbol, found EOF".to_string()).into());
        }
    }

    pub fn parse(lexer: impl Iterator<Item = Token>) -> Result<Self> {
        Self::parse_peekable(&mut lexer.peekable())
    }
}

impl TryFrom<&str> for Expr {
    type Error = anyhow::Error;
    fn try_from(s: &str) -> Result<Self> {
        Expr::parse(crate::lexer::Lexer::from_iter(s.chars().peekable()))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name),
            Expr::Fun(name, args) => {
                write!(f, "{}(", name)?;
                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}
