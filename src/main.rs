use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
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

#[derive(Debug, Clone)]
struct Rule {
    head: Expr,
    body: Expr,
}

impl Rule {
    fn apply_all(&self, expr: &Expr) -> Expr {
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

type Bindings = HashMap<String, Expr>;

fn substitute_bindings(bindings: &Bindings, expr: &Expr) -> Expr {
    match expr {
        Expr::Sym(name) => bindings.get(name).unwrap_or(expr).clone(),
        Expr::Fun(name, args) => {
            let new_name = match bindings.get(name) {
                Some(Expr::Sym(new_name)) => new_name.clone(),
                None => name.clone(),
                _ => panic!(),
            };
            Expr::Fun(
                new_name,
                args.iter()
                    .map(|arg| substitute_bindings(bindings, arg))
                    .collect(),
            )
        }
    }
}

fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    fn matches(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
        match (pattern, value) {
            (Expr::Sym(name), _) => {
                if let Some(existing) = bindings.get(name) {
                    existing == value
                } else {
                    bindings.insert(name.clone(), value.clone());
                    true
                }
            }
            (Expr::Fun(pat_name, pat_args), Expr::Fun(val_name, val_args)) => {
                if pat_name == val_name && pat_args.len() == val_args.len() {
                    pat_args
                        .iter()
                        .zip(val_args.iter())
                        .all(|(pat_arg, val_arg)| matches(pat_arg, val_arg, bindings))
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    let mut bindings = Bindings::new();

    if matches(pattern, value, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

macro_rules! s {
    ($s:expr) => {
        $s.to_string()
    };
}

fn main() {
    use Expr::*;

    // swap(pair(a, b)) = pair(b, a)
    let swap = Rule {
        head: Fun(
            s!("swap"),
            vec![Fun(s!("pair"), vec![Sym(s!("a")), Sym(s!("b"))])],
        ),
        body: Fun(s!("pair"), vec![Sym(s!("b")), Sym(s!("a"))]),
    };

    let expr = Fun(
        s!("swap"),
        vec![Fun(
            s!("pair"),
            vec![
                Fun(s!("f"), vec![Sym(s!("a"))]),
                Fun(s!("g"), vec![Sym(s!("b"))]),
            ],
        )],
    );
    let expr2 = Fun(
        s!("swap"),
        vec![Fun(
            s!("pair"),
            vec![
                Fun(s!("q"), vec![Sym(s!("c"))]),
                Fun(s!("z"), vec![Sym(s!("d"))]),
            ],
        )],
    );
    let foo = Fun(s!("foo"), vec![expr, expr2]);

    println!("Rule: {}", swap);
    println!("Expr: {}", foo);
    println!("   => {}", swap.apply_all(&foo));
}

#[cfg(test)]
mod tests {
    use super::pattern_match;
    use super::Expr::*;
    use super::Rule;

    #[test]
    fn matching() {
        // swap(pair(a, b)) = pair(b, a)
        let swap = Rule {
            head: Fun(
                s!("swap"),
                vec![Fun(s!("pair"), vec![Sym(s!("a")), Sym(s!("b"))])],
            ),
            body: Fun(s!("pair"), vec![Sym(s!("b")), Sym(s!("a"))]),
        };

        // Pattern: swap(pair(a, b))
        let pattern = swap.head.clone();
        // swap(pair(f(c), g(d)))
        let value = Fun(
            s!("swap"),
            vec![Fun(
                s!("pair"),
                vec![
                    Fun(s!("f"), vec![Sym(s!("c"))]),
                    Fun(s!("g"), vec![Sym(s!("d"))]),
                ],
            )],
        );
        // same shape? return hashmap of bindings

        if let Some(bindings) = pattern_match(&pattern, &value) {
            eprintln!("MATCH:");
            for (name, value) in bindings {
                eprintln!("{} = {}", name, value);
            }
        } else {
            eprintln!("{}", pattern);
            eprintln!("{}", value);
            panic!("No match");
        }
    }
}
