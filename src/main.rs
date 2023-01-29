#![feature(box_syntax)]
#![feature(panic_info_message)]

use std::env::args;

use self::context::Context;

mod bindings;
mod context;
mod lexer;
mod repl;
mod rule;
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
            crate::rule::Expr::Var(stringify!($name).to_string())
        } else {
            crate::rule::Expr::Sym(stringify!($name).to_string())
        }
    };
    ($name:ident($($args:tt)*)) => {
        crate::rule::Expr::Fun(box expr!($name), fun_args!( $($args)* ))
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
        let source = std::fs::read_to_string(file).unwrap();
        let mut runner = Context::new(lexer::Lexer::from_iter(source.chars().peekable()));

        while !runner.lexer.peek().is_none() {
            match runner.run_cmd() {
                Ok(Some(output)) => println!(" => {}", output),
                Ok(None) => (),
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
