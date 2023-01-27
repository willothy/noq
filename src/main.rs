use self::{
    lexer::{Lexer, Token},
    rule::Expr,
};

mod bindings;
mod lexer;
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
    ($name:ident) => {
        crate::rule::Expr::Sym(stringify!($name).to_string())
    };
    ($name:ident($($args:tt)*)) => {
        crate::rule::Expr::Fun(stringify!($name).to_string(), fun_args!( $($args)* ))
    };
}

fn main() {
    let lexer = Lexer::from("f(x) = g(x, y)");
    let expr = Expr::parse(lexer);
    println!("{}", expr);
}
