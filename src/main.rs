use std::{
    collections::HashMap,
    io::{stdin, stdout, Write},
};

use crate::{lexer::Lexer, rule::Expr};

use self::rule::Rule;

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

pub struct ReplState {
    rules: HashMap<String, Rule>,
    shape: Option<Expr>,
}

fn main() {
    let mut state = ReplState {
        rules: HashMap::new(),
        shape: None,
    };
    let mut input_buf = String::new();
    let mut quit = false;

    while !quit {
        print!("> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input_buf).unwrap();
        let input = input_buf.drain(..).collect::<String>();

        let (cmd, rest) = {
            let mut split = input.split_whitespace();
            let Some(cmd) = split.next() else {
                // No command given
                continue;
            };
            (cmd, split.collect::<Vec<&str>>().join(" "))
        };

        match cmd {
            "quit" | "q" => {
                quit = true;
                continue;
            }
            "shape" => {
                if rest.trim().is_empty() {
                    if let Some(shape) = &state.shape {
                        println!(" => {}", shape);
                    } else {
                        println!("No shape defined");
                    }
                    continue;
                }
                let shape = Expr::parse(Lexer::from(&*rest));
                state.shape = Some(shape.clone());
                println!(" => {}", shape);
            }
            "rule" => {
                let name = rest.trim().split_whitespace().next().unwrap().to_owned();
                let rule = Rule::parse(Lexer::from(&rest[name.len()..]));
                state.rules.insert(name, rule);
            }
            "apply" => {
                let rule_name = rest.split_whitespace().next().unwrap().to_owned();
                let Some(rule) = state.rules.get(&rule_name) else {
                    println!("Unknown rule {}", rule_name);
                    continue;
                };
                let new_shape = rule.apply_all({
                    let Some(shape) = &state.shape else {
                        println!("No shape defined");
                        continue;
                    };
                    shape
                });
                println!(" => {}", new_shape);
                state.shape = Some(new_shape);
            }
            _ => println!("Unknown command"),
        }
    }
}
