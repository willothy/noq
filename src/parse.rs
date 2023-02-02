use std::env;
use std::path::PathBuf;

use crate::error::{common::*, ParseError::*};
use crate::expr::Expr;
use crate::lexer::{self, Lexer, Loc, StrategyKind};
use crate::lexer::{Token, TokenKind};
use crate::rule::Rule;
use crate::{err, err_hl};

pub(crate) fn parse_use(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<(String, Loc)> {
    lexer.catchup();

    match lexer.next() {
        Some(t) => match t {
            Token {
                kind: TokenKind::Ident,
                text,
                loc,
                ..
            } => Ok((text, loc)),
            Token {
                kind: TokenKind::String,
                text,
                loc,
                ..
            } => Ok((text, loc)),
            Token {
                kind: TokenKind::Path,
                text,
                loc,
                ..
            } => Ok((text, loc)),
            invalid => {
                err!(Parse UnexpectedToken(err_hl!(invalid.text)), invalid.loc)
            }
        },
        None => err!(Parse UnexpectedEOF, lexer.current_loc()).with_message("Expected file name"),
    }
}

pub(crate) enum SaveType {
    All,
    Commands,
    Rules,
}

pub(crate) fn parse_save(
    lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<(SaveType, String, Loc)> {
    lexer.catchup();
    let save_type = match lexer.next() {
        Some(tok) => match tok.kind {
            TokenKind::Rules => SaveType::Rules,
            TokenKind::Commands => SaveType::Commands,
            TokenKind::Strategy(StrategyKind::ApplyAll) => SaveType::All,
            _ => {
                return err!(Parse UnexpectedToken(err_hl!(tok.text)), tok.loc)
                    .with_message("Expected save type (all, commands, or rules)")
            }
        },
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected save type (all, commands, rules) and file name")
        }
    };
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
                return err!(Parse UnexpectedToken(err_hl!(invalid.text)), invalid.loc)
                    .with_message("Expected file name")
            }
        },
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected file name")
        }
    };
    Ok((save_type, file_name, loc))
}

pub(crate) fn parse_run(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<(String, Loc)> {
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
                return err!(Parse UnexpectedToken(err_hl!(invalid.text)), invalid.loc)
                    .with_message("Expected file name")
            }
        },
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected file name")
        }
    };
    Ok((file_name, loc))
}

pub(crate) fn parse_anon_rule(
    mut lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<(Rule, StrategyKind, bool, String)> {
    lexer.catchup();
    let head = Expr::parse(&mut lexer)?;
    let rule = if lexer.next_if(|x| x.kind == TokenKind::Equals).is_some() {
        let body = Expr::parse(&mut lexer)?;

        Rule { head, body }
    } else {
        let bad = lexer.next_token();
        return err!(Parse UnexpectedToken(err_hl!(bad.text)), bad.loc)
            .with_message("anon rule expected '='");
    };
    if !lexer.next_if(|x| x.kind == TokenKind::Bar).is_some() {
        let bad = lexer.next_token();
        return err!(Parse UnexpectedToken(err_hl!(bad.text)), bad.loc)
            .with_message("anon rule expected '|'");
    }
    let reversed = lexer.next_if(|x| x.kind == TokenKind::Bang).is_some();
    let (strategy, n) = match lexer.next() {
        Some(lexer::Token {
            kind: TokenKind::Strategy(strategy),
            text,
            ..
        }) => (strategy, text),
        Some(invalid) => {
            return err!(Parse InvalidStrategy(invalid.text), invalid.loc)
                .with_message("Expected strategy name")
        }
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected strategy name");
        }
    };
    Ok((rule, strategy, reversed, n))
}

pub(crate) fn parse_all_rules(
    lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<(bool, StrategyKind, String)> {
    lexer.catchup();
    if !lexer.next_if(|x| x.kind == TokenKind::Bar).is_some() {
        let bad = lexer.next_token();
        return err!(Parse UnexpectedToken(err_hl!(bad.text)), bad.loc)
            .with_message("Expected '|'");
    }
    let reverse = lexer.next_if(|x| x.kind == TokenKind::Bang).is_some();
    let (strategy, n) = match lexer.next() {
        Some(lexer::Token {
            kind: TokenKind::Strategy(strategy),
            text,
            ..
        }) => (strategy, text),
        Some(invalid) => {
            return err!(Parse InvalidStrategy(err_hl!(invalid.text)), invalid.loc)
                .with_message("Expected strategy name");
        }
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected strategy name");
        }
    };
    Ok((reverse, strategy, n))
}

pub(crate) enum RuleDefResult {
    Def(Rule),
    DefViaShape(Expr),
}

pub(crate) fn parse_def_rule(
    mut lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<RuleDefResult> {
    use RuleDefResult::*;
    lexer.catchup();
    // Rule definition
    let head = Expr::parse(&mut lexer)?;
    let next = lexer.next_token();
    match next.kind {
        TokenKind::OpenBrace | TokenKind::Eof => {
            // Start shaping for rule
            Ok(DefViaShape(head))
        }
        TokenKind::Equals => Ok(Def(Rule {
            head,
            body: Expr::parse(&mut lexer)?,
        })),
        _ => err!(Parse UnexpectedToken(next.text), next.loc)
            .with_message("Expected open brace or equals"),
    }
}

pub(crate) fn parse_apply_named_rule(
    lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<(bool, StrategyKind, String)> {
    lexer.catchup();
    let reverse = lexer.next_if(|t| t.kind == TokenKind::Bang).is_some();

    let (strategy, n) = match lexer.next() {
        Some(lexer::Token {
            kind: TokenKind::Strategy(strategy),
            text,
            ..
        }) => (strategy, text),
        Some(invalid) => {
            return err!(Parse InvalidStrategy(err_hl!(invalid.text)), invalid.loc)
                .with_message("Expected strategy name");
        }
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected strategy name");
        }
    };
    Ok((reverse, strategy, n))
}

pub(crate) fn parse_eval(
    lexer: &mut Lexer<impl Iterator<Item = char>>,
) -> Result<(StrategyKind, String)> {
    lexer.catchup();
    if let Some(t) = lexer.next_if(|t| t.kind == TokenKind::Bang) {
        return err!(Parse UnexpectedToken(err_hl!("!")), t.loc)
            .with_message("cannot reverse an eval application");
    }
    let (strategy, n) = match lexer.next() {
        Some(lexer::Token {
            kind: TokenKind::Strategy(strategy),
            text,
            ..
        }) => (strategy, text),
        Some(invalid) => {
            return err!(Parse InvalidStrategy(err_hl!(invalid.text)), invalid.loc)
                .with_message("Expected strategy name");
        }
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc())
                .with_message("Expected strategy name");
        }
    };
    Ok((strategy, n))
}

pub(crate) fn parse_def_shape(mut lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Expr> {
    let shape = Expr::parse(&mut lexer)?;
    if lexer
        .next_if(|tok| tok.kind == TokenKind::OpenBrace)
        .is_some()
        || lexer.next_if(|tok| tok.kind == TokenKind::Eof).is_some()
    {
        Ok(shape)
    } else {
        let invalid = lexer.next().unwrap();
        return err!(Parse UnexpectedToken(err_hl!(invalid.text)), invalid.loc)
            .with_message("Expected open brace or EOL");
    }
}

pub(crate) fn parse_pwd(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<PathBuf> {
    lexer.catchup();
    env::current_dir()
        .inherit(lexer.current_loc())
        .with_prefix("Cannot get current directory: ")
}

pub(crate) fn parse_cd(lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<(String, Loc)> {
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
                return err!(Parse UnexpectedToken(err_hl!(invalid.text)), invalid.loc)
                    .with_message("Expected path");
            }
        },
        None => {
            return err!(Parse UnexpectedEOF, lexer.current_loc()).with_message("Expected path")
        }
    };
    Ok((path, loc))
}
