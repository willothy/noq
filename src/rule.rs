use std::{fmt::Display, iter::Peekable};

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
    pub fn apply_all(&self, expr: &Expr) -> Expr {
        if let Some(bindings) = pattern_match(&self.head, expr) {
            substitute_bindings(&bindings, &self.body)
        } else {
            match expr {
                Expr::Sym(_) => expr.clone(),
                Expr::Fun(name, args) => Expr::Fun(
                    name.clone(),
                    args.iter().map(|arg| self.apply_all(arg)).collect(),
                ),
            }
        }
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
    Fun(String, Vec<Expr>),
}

impl Expr {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        if let Some(name) = lexer.next() {
            if let TokenKind::Sym = name.kind {
                if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                    todo!("args")
                } else {
                    Expr::Sym(name.text)
                }
            } else {
                todo!("Expected symbol")
            }
        } else {
            todo!("EOF Error")
        }
    }

    pub fn parse(lexer: impl Iterator<Item = Token>) -> Self {
        Self::parse_peekable(&mut lexer.peekable())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Sym(name) => write!(f, "{}", name),
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
