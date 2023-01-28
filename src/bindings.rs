use std::collections::HashMap;

use crate::rule::Expr;

pub(crate) type Bindings = HashMap<String, Expr>;

pub(crate) fn substitute_bindings(bindings: &Bindings, expr: &Expr) -> Result<Expr, String> {
    Ok(match expr {
        Expr::Sym(name) => bindings.get(name).unwrap_or(expr).clone(),
        Expr::Fun(name, args) => {
            let new_name = match bindings.get(name) {
                Some(Expr::Sym(new_name)) => new_name.clone(),
                None => name.clone(),
                _ => return Err("Invalid substitution".into()),
            };
            Expr::Fun(
                new_name,
                args.iter()
                    .map(|arg| substitute_bindings(bindings, arg))
                    .collect::<Result<Vec<_>, String>>()?,
            )
        }
    })
}

pub(crate) fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
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
