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
    pub fn parse(lexer: impl Iterator<Item = Token>) -> Self {
        let mut lexer = lexer.peekable();
        let head = Expr::parse_peekable(&mut lexer);
        lexer.next_if(|t| t.kind == TokenKind::Equals).unwrap();
        let body = Expr::parse_peekable(&mut lexer);
        Self { head, body }
    }
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
                    let mut args = Vec::new();
                    if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::CloseParen) {
                        return Expr::Fun(name.text, args);
                    }
                    args.push(Self::parse_peekable(lexer));
                    while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Comma) {
                        args.push(Self::parse_peekable(lexer));
                    }
                    if lexer.next_if(|t| t.kind == TokenKind::CloseParen).is_none() {
                        todo!("Expected close paren");
                    }
                    Expr::Fun(name.text, args)
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

impl<T> From<T> for Expr
where
    T: Into<String>,
{
    fn from(s: T) -> Self {
        Expr::parse(crate::lexer::Lexer::from_iter(s.into().chars()))
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
