use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::{
    lexer::{self, Token, TokenKind},
    rule::{self, Expr, Rule},
};
use anyhow::Result;

pub struct Runner<'a> {
    lexer: Peekable<lexer::Lexer<Chars<'a>>>,
    rules: HashMap<String, Rule>,
    shape: Option<Expr>,
}

impl<'a> Runner<'a> {
    pub fn run_file(source: &'a str) -> Result<()> {
        let mut runner = Self {
            lexer: lexer::Lexer::from(source).peekable(),
            rules: HashMap::new(),
            shape: None,
        };

        loop {
            match runner.lexer.next() {
                Some(lexer::Token {
                    kind: lexer::TokenKind::Rule,
                    ..
                }) => {
                    let Some(Token {
                        kind: lexer::TokenKind::Sym,
                        text: name,
                    }) = runner.lexer.next() else {
                        return Err(anyhow::anyhow!("Expected rule name"));
                    };
                    runner.rules.insert(name, Rule::parse(&mut runner.lexer)?);
                }
                Some(lexer::Token {
                    kind: lexer::TokenKind::Apply,
                    ..
                }) => {
                    if let Some(shape) = &mut runner.shape {
                        if let Some(lexer::Token {
                            kind: lexer::TokenKind::Rule,
                            ..
                        }) = runner.lexer.peek()
                        {
                            runner.lexer.next();
                            *shape = rule::Rule::parse(&mut runner.lexer)?.apply_all(&shape)?;
                        } else if let Some(lexer::Token {
                            kind: lexer::TokenKind::Sym,
                            text: name,
                        }) = runner.lexer.next()
                        {
                            *shape = runner
                                .rules
                                .get(&name)
                                .ok_or(anyhow::anyhow!("No rule named {}", name))?
                                .apply_all(&shape)?;
                        } else {
                            return Err(anyhow::anyhow!("Expected rule name or anonymous rule"));
                        }
                    } else {
                        return Err(anyhow::anyhow!("No shape to apply to"));
                    }
                }
                Some(lexer::Token {
                    kind: lexer::TokenKind::Shape,
                    ..
                }) => {
                    runner.shape = Some(rule::Expr::parse(&mut runner.lexer)?);
                    while runner
                        .lexer
                        .next_if(|x| matches!(x.kind, TokenKind::Done))
                        .is_none()
                    {
                        if let Some(shape) = &mut runner.shape {
                            *shape = Expr::parse(&mut runner.lexer)?;
                        } else {
                            return Err(anyhow::anyhow!("No shape to apply to"));
                        }
                    }
                }
                Some(invalid) => return Err(anyhow::anyhow!("Invalid command {:?}", invalid)),
                None => break,
            }
        }
        Ok(())
    }
}
