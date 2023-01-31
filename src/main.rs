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

#[macro_export]
macro_rules! fun_args {
    () => {
        vec![]
    };
    ($name:ident) => {
        vec![expr!($name)]
    };
    ($name:ident, $($rest:tt)*) => {
        {
            let mut t = vec![expr!( $name )];
            t.append(&mut fun_args!( $($rest)* ));
            t
        }
    };
    ($name:ident($($args:tt)*)) => {
        vec![expr!($name($($args)*))]
    };
    ($name:ident($($args:tt)*), $($rest:tt)*) => {
        {
            let mut t = vec![expr!($name($($args)*))];
            t.append(&mut fun_args!( $($rest)* ));
            t
        }
    };
}

#[macro_export]
macro_rules! expr {
    ($source:literal) => {
        crate::rule::Expr::parse(crate::lexer::Lexer::new($source))
    };
    ($name:ident) => {
        if stringify!($name).chars().nth(0).unwrap().is_uppercase() {
            crate::expr::Expr::Var(stringify!($name).to_string())
        } else {
            crate::expr::Expr::Sym(stringify!($name).to_string())
        }
    };
    ($name:ident($($args:tt)*)) => {
        crate::expr::Expr::Fun(box expr!($name), fun_args!( $($args)* ))
    };
}

#[macro_export]
macro_rules! rule {
    ($head:ident = $body:tt) => {
        crate::rule::Rule {
            head: expr!($head),
            body: expr!($body),
        }
    };
    ($head:ident = $body:ident($($body_args:tt)*)) => {
        crate::rule::Rule {
            head: expr!($head),
            body: expr!($body($($body_args)*)),
        }
    };
    ($head:ident($($args:tt)*) = $body:tt) => {
        crate::rule::Rule {
            head: expr!($head($($args)*)),
            body: expr!($body),
        }
    };
    ($head:ident($($args:tt)*) = $body:ident($($body_args:tt)*)) => {
        crate::rule::Rule {
            head: expr!($head($($args)*)),
            body: expr!($body($($body_args)*)),
        }
    };
}

fn collect_subexprs<'a>(pattern: &'a Expr, expr: &'a Expr) -> Vec<&'a Expr> {
    let mut subexprs = vec![];

    fn match_all_inner<'a>(pattern: &'a Expr, expr: &'a Expr, subexprs: &mut Vec<&'a Expr>) {
        if pattern_match(pattern, expr).is_some() {
            subexprs.push(expr);
        }

        match expr {
            Expr::Fun(head, args) => {
                match_all_inner(pattern, head, subexprs);
                for arg in args {
                    match_all_inner(pattern, arg, subexprs);
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
        Expr::Fun(head, args) => {
            if highlight {
                write_subexpr_highlighted(head, subexprs, idx, style, highlight, writer)?;
                write!(writer, "{}", style.apply("("))?;
                for (idx, arg) in args.iter().enumerate() {
                    write_subexpr_highlighted(arg, subexprs, idx, style, highlight, writer)?;
                    if idx + 1 < args.len() {
                        write!(writer, "{}", style.apply(", "))?;
                    }
                }
                write!(writer, "{}", style.apply(")"))
            } else {
                write_subexpr_highlighted(head, subexprs, idx, style, highlight, writer)?;
                write!(writer, "(")?;
                for arg in args {
                    write_subexpr_highlighted(arg, subexprs, idx, style, highlight, writer)?;
                    if idx + 1 < args.len() {
                        write!(writer, "{}", ", ")?;
                    }
                }
                write!(writer, ")")
            }
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
