#![feature(box_syntax)]
#![feature(panic_info_message)]
#![feature(try_trait_v2)]

use std::{env::args, fmt, path::PathBuf};

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

fn write_subexpr_highlighted(
    expr: &Expr,
    subexprs: &Vec<&Expr>,
    idx: usize,
    style: ContentStyle,
    parent_highlight: bool,
    writer: &mut dyn std::fmt::Write,
) -> fmt::Result {
    let highlight = pattern_match(expr, subexprs[idx]).is_some() || parent_highlight;
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
                write!(writer, "{}", style.apply(")"))
            } else {
                write!(writer, "{}", "(")?;
                for (i, element) in elements.iter().enumerate() {
                    write_subexpr_highlighted(element, subexprs, idx, style, highlight, writer)?;
                    if i + 1 < elements.len() {
                        write!(writer, "{}", ", ")?;
                    }
                }
                write!(writer, "{}", ")")
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
                write!(writer, "{} {} ", style.apply(" "), style.apply(op))?;
                write_subexpr_highlighted(rhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, "{}", style.apply(")"))
            } else {
                write!(writer, "(")?;
                write_subexpr_highlighted(lhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, " {} ", op)?;
                write_subexpr_highlighted(rhs, subexprs, idx, style, highlight, writer)?;
                write!(writer, ")")
            }
        }
        Expr::Sym(sym) => {
            if highlight {
                write!(writer, "{}", style.apply(sym))
            } else {
                write!(writer, "{}", sym)
            }
        }
        Expr::Var(var) => {
            if highlight {
                write!(writer, "{}", style.apply(var))
            } else {
                write!(writer, "{}", var)
            }
        }
        Expr::Num(num) => {
            if highlight {
                write!(writer, "{}", style.apply(num))
            } else {
                write!(writer, "{}", num)
            }
        }
        Expr::Str(str) => {
            if highlight {
                write!(writer, "\"{}\"", style.apply(str))
            } else {
                write!(writer, "\"{}\"", str)
            }
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

    let pattern = Expr::try_from("_(_)").unwrap();
    let pattern2 = Expr::try_from("_").unwrap();
    let expr = Expr::try_from("f(g(a), b, c)").unwrap();
    let subexprs = collect_subexprs(&pattern, &expr);
    let subexprs2 = collect_subexprs(&pattern2, &expr);
    let mut s = String::new();
    write_subexpr_highlighted(
        &expr,
        &subexprs,
        0,
        ContentStyle::new().with(crossterm::style::Color::Green),
        false,
        &mut s,
    )
    .unwrap();
    s += "\n";
    write_subexpr_highlighted(
        &expr,
        &subexprs2,
        0,
        ContentStyle::new().with(crossterm::style::Color::Green),
        false,
        &mut s,
    )
    .unwrap();
    println!("{}", s);

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
