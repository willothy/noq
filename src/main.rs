#![feature(box_syntax)]

use std::process::ExitCode;

mod bindings;
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
        crate::rule::Expr::Sym(stringify!($name).to_string())
    };
    ($name:ident($($args:tt)*)) => {
        crate::rule::Expr::Fun(stringify!($name).to_string(), fun_args!( $($args)* ))
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

fn main() -> ExitCode {
    repl::Repl::run();
    ExitCode::SUCCESS
}
