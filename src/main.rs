#![feature(box_syntax)]
#![feature(panic_info_message)]
#![feature(try_trait_v2)]

use std::{env::args, fmt, path::PathBuf};

use anyhow::anyhow;
use crossterm::{
    execute,
    style::{Attribute, ContentStyle, Stylize},
};

use crate::rule::pattern_match;

use self::{
    expr::Expr,
    lexer::Lexer,
    runtime::{Runtime, StepResult, Verbosity},
};

mod expr;
mod lexer;
mod repl;
mod rule;
mod runtime;
mod tests;

fn collect_subexprs<'a>(pattern: &'a Expr, expr: &'a Expr) -> Vec<&'a Expr> {
    let mut subexprs = vec![];

    fn match_all_inner<'a>(pattern: &'a Expr, expr: &'a Expr, subexprs: &mut Vec<&'a Expr>) {
        if pattern_match(pattern, expr).is_some() {
            subexprs.push(expr);
        }

        match expr {
            Expr::Fun(head, body) => {
                match_all_inner(pattern, head, subexprs);
                match_all_inner(pattern, body, subexprs)
            }
            Expr::List(elements) => {
                for element in elements {
                    match_all_inner(pattern, element, subexprs);
                }
            }
            Expr::Op(_, lhs, rhs) => {
                match_all_inner(pattern, lhs, subexprs);
                match_all_inner(pattern, rhs, subexprs);
            }
            _ => {}
        }
    }

    match_all_inner(pattern, expr, &mut subexprs);

    subexprs
}

fn collect_sub_constexprs<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
    let mut subexprs = vec![];

    fn inner<'a>(expr: &'a Expr, subexprs: &mut Vec<&'a Expr>) {
        if expr.is_const_expr() && !expr.is_num() {
            subexprs.push(expr);
        }

        match expr {
            Expr::Fun(head, body) => {
                inner(head, subexprs);
                inner(body, subexprs);
            }
            Expr::List(elements) => {
                for element in elements {
                    inner(element, subexprs);
                }
            }
            Expr::Op(_, lhs, rhs) => {
                inner(lhs, subexprs);
                inner(rhs, subexprs);
            }
            _ => {}
        }
    }

    inner(expr, &mut subexprs);

    subexprs
}

fn write_subexpr_highlighted(
    expr: &Expr,
    subexprs: &Vec<&Expr>,
    idx: usize,
    style: ContentStyle,
    parent_highlight: bool,
    writer: &mut dyn std::fmt::Write,
) -> anyhow::Result<()> {
    let subexpr = subexprs
        .get(idx)
        .ok_or(anyhow!("Subexpr {} does not exist", idx))?;
    let highlight = pattern_match(expr, subexpr).is_some() || parent_highlight;
    match expr {
        Expr::List(elements) => {
            if highlight {
                write!(writer, "{}", style.apply("("))?;
                for (i, element) in elements.iter().enumerate() {
                    write_subexpr_highlighted(element, subexprs, idx, style, highlight, writer)?;
                    if i + 1 < elements.len() {
                        write!(writer, "{}", style.apply(", "))?;
                    }
                }
                write!(writer, "{}", style.apply(")"))?;
                Ok(())
            } else {
                write!(writer, "{}", "(")?;
                for (i, element) in elements.iter().enumerate() {
                    write_subexpr_highlighted(element, subexprs, idx, style, highlight, writer)?;
                    if i + 1 < elements.len() {
                        write!(writer, "{}", ", ")?;
                    }
                }
                write!(writer, "{}", ")")?;
                Ok(())
            }
        }
        Expr::Fun(head, body) => {
            if highlight {
                write_subexpr_highlighted(head, subexprs, idx, style, highlight, writer)?;
                write_subexpr_highlighted(body, subexprs, idx, style, highlight, writer)?;
            } else {
                write_subexpr_highlighted(head, subexprs, idx, style, highlight, writer)?;
                write_subexpr_highlighted(body, subexprs, idx, style, highlight, writer)?;
            }
            Ok(())
        }
        Expr::Op(op, lhs, rhs) => {
            if highlight {
                write!(writer, "{}", style.apply("("))?;
                write_subexpr_highlighted(lhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, " {} ", style.apply(op))?;
                write_subexpr_highlighted(rhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, "{}", style.apply(")"))?;
                Ok(())
            } else {
                write!(writer, "(")?;
                write_subexpr_highlighted(lhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, " {} ", op)?;
                write_subexpr_highlighted(rhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, ")")?;
                Ok(())
            }
        }
        Expr::Sym(sym) => {
            if highlight {
                write!(writer, "{}", style.apply(sym))?;
            } else {
                write!(writer, "{}", sym)?;
            }
            Ok(())
        }
        Expr::Var(var, _) => {
            if highlight {
                write!(writer, "{}", style.apply(var))?;
            } else {
                write!(writer, "{}", var)?;
            }
            Ok(())
        }
        Expr::Num(num) => {
            if highlight {
                write!(writer, "{}", style.apply(num))?;
            } else {
                write!(writer, "{}", num)?;
            }
            Ok(())
        }
        Expr::Str(str) => {
            if highlight {
                write!(writer, "\"{}\"", style.apply(str))?;
            } else {
                write!(writer, "\"{}\"", str)?;
            }
            Ok(())
        }
    }
}

fn main() -> anyhow::Result<()> {
    std::panic::set_hook(Box::new(|info| {
        crossterm::terminal::disable_raw_mode().unwrap();
        println!("\nThe program has panicked. Please report this to https://github.com/willothy/noq/issues");
        if let Some(location) = info.location() {
            if let Some(payload) = info.message() {
                println!("Panicked with \"{}\" at {}", payload, location);
                return;
            }
            println!("Panicked with no message at {}", location);
        }
    }));

    if let Some(file) = args().nth(1) {
        let path = PathBuf::from(file);
        let source = std::fs::read_to_string(&path).unwrap();
        let mut lexer = Lexer::new(source.chars().peekable())
            .with_file_name(path.file_name().unwrap().to_str().unwrap().to_string());

        let mut runtime = Runtime::new();
        runtime.verbosity = Verbosity::Normal;

        while !lexer.exhausted {
            match runtime.step(&mut lexer) {
                Ok(StepResult {
                    results: Some(output),
                    cmd_for_each: cmd_per,
                    clear,
                    ..
                }) => {
                    if clear {
                        execute!(
                            std::io::stdout(),
                            crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
                        )
                        .unwrap();
                    }
                    for (idx, out) in output.iter().enumerate() {
                        for line in out {
                            if cmd_per {
                                println!(" => {}", line);
                            } else {
                                if idx == 0 {
                                    println!("=> {}", line);
                                } else {
                                    println!("{}", line);
                                }
                            }
                        }
                    }
                }
                Ok(StepResult { results: None, .. }) => (),
                Err(e) => {
                    println!(" !> {}", e);
                    break;
                }
            }
        }
    } else {
        repl::Repl::run();
    }
    Ok(())
}
