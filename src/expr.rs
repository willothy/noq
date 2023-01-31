use std::fmt::Display;

use anyhow::{bail, Result};
use thiserror::Error;

use crate::lexer::{Lexer, OpKind, StringUnwrap, Token, TokenKind, MAX_PRECEDENCE};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Sym(String),
    Var(String),
    Num(i64),
    Str(String),
    Fun(Box<Expr>, Box<Expr>),
    Op(OpKind, Box<Expr>, Box<Expr>),
    List(Vec<Expr>),
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
            .filter(|c| c.is_uppercase() || *c == '_')
            .map(|_| Expr::Var(name.to_owned()))
            .unwrap_or(Expr::Sym(name.to_owned())))
    }

    fn parse_list(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Vec<Self>> {
        use TokenKind::*;
        if lexer.next_if(|tok| tok.kind == OpenParen).is_none() {
            return Err(ParseError(format!(
                "Expected '(', got {}",
                lexer.next().unwrap_string()
            ))
            .into());
        }
        let mut args = Vec::new();
        if lexer.next_if(|tok| tok.kind == CloseParen).is_some() {
            return Ok(args);
        }
        args.push(Self::parse(lexer)?);
        while lexer.next_if(|tok| tok.kind == Comma).is_some() {
            args.push(Self::parse(lexer)?);
        }
        if lexer.next_if(|tok| tok.kind == CloseParen).is_none() {
            return Err(ParseError(format!(
                "Expected ')', got {}",
                lexer.next().unwrap_string()
            ))
            .into());
        }
        Ok(args)
    }

    fn parse_fn_or_var_or_sym(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self> {
        let mut head = {
            match lexer.next_token() {
                Token {
                    kind: TokenKind::OpenParen,
                    ..
                } => {
                    let result = Self::parse(lexer)?;
                    /* if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                        return Err(ParseError(format!(
                            "Expected ')', got {}",
                            lexer.next().unwrap_string()
                        ))
                        .into());
                    }
                    result */
                    if lexer.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                        let mut result = vec![result, Self::parse(lexer)?];
                        while lexer.next_if(|t| t.kind == TokenKind::Comma).is_some() {
                            result.push(Self::parse(lexer)?);
                            println!("peek: {:?}", lexer.peek());
                        }
                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            return Err(ParseError(format!(
                                "Expected ')', got {}",
                                lexer.next().unwrap_string()
                            ))
                            .into());
                        }
                        Expr::List(result)
                    } else {
                        if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                            return Err(ParseError(format!(
                                "Expected ')', got {}",
                                lexer.next().unwrap_string()
                            ))
                            .into());
                        }
                        result
                    }
                }
                Token {
                    kind: TokenKind::Ident,
                    text,
                    ..
                } => Self::var_or_sym(&text)?,
                Token {
                    kind: TokenKind::Number,
                    text,
                    ..
                } => Expr::Num(text.parse()?),
                Token {
                    kind: TokenKind::String,
                    text,
                    ..
                } => {
                    if text.is_empty() {
                        return Err(ParseError("Empty string".to_owned()).into());
                    }
                    Expr::Str(text)
                }
                t => return Err(ParseError(format!("Expected symbol, found {}", t)).into()),
            }
        };

        while let Token {
            kind: TokenKind::OpenParen,
            ..
        } = lexer.peek()
        {
            head = Expr::Fun(box head, box Expr::List(Self::parse_list(lexer)?));
        }
        Ok(head)
    }

    fn parse_binop(
        lexer: &mut Lexer<impl Iterator<Item = char>>,
        precedence: usize,
    ) -> Result<Self> {
        if precedence > MAX_PRECEDENCE {
            let res = Self::parse_fn_or_var_or_sym(lexer);
            return res;
        }

        let mut result = Self::parse_binop(lexer, precedence + 1)?;
        while let Some(Token {
            kind: TokenKind::Op(op),
            ..
        }) = lexer.next_if(|tok| match &tok.kind {
            TokenKind::Op(op) => op.precedence() == precedence,
            _ => false,
        }) {
            let rhs = Self::parse_binop(lexer, precedence)?;
            result = Expr::Op(op, box result, box rhs);
        }

        Ok(result)
    }

    pub fn parse(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self> {
        Self::parse_binop(lexer, 0)
    }
}

impl TryFrom<&str> for Expr {
    type Error = anyhow::Error;
    fn try_from(s: &str) -> Result<Self> {
        Expr::parse(&mut crate::lexer::Lexer::new(s.chars().peekable()))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::List(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name),
            Expr::Fun(head, body) => {
                match &**head {
                    Expr::Sym(name) | Expr::Var(name) => write!(f, "{}", name)?,
                    other => write!(f, "({})", other)?,
                }
                write!(f, "(")?;
                match &**body {
                    Expr::List(exprs) => {
                        for (i, expr) in exprs.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", expr)?;
                        }
                    }
                    other => write!(f, "{}", other)?,
                }
                write!(f, ")")
            }
            Expr::Op(op, lhs, rhs) => {
                match lhs.as_ref() {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(f, "({})", lhs)?
                        } else {
                            write!(f, "{}", lhs)?
                        }
                    }
                    _ => write!(f, "{}", lhs)?,
                }
                if op.precedence() == 0 {
                    write!(f, " {} ", op)?;
                } else {
                    write!(f, "{}", op)?;
                }
                match rhs.as_ref() {
                    Expr::Op(sub_op, _, _) => {
                        if sub_op.precedence() <= op.precedence() {
                            write!(f, "({})", rhs)
                        } else {
                            write!(f, "{}", rhs)
                        }
                    }
                    _ => write!(f, "{}", rhs),
                }
            }
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}
