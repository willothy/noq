use std::collections::HashMap;

use crate::rule::Expr;
use anyhow::Result;
use thiserror::Error;

pub(crate) type Bindings = HashMap<String, Expr>;

#[derive(Debug, Error)]
#[error("Match error: {0}")]
pub struct MatchError<'a>(&'a str);

pub(crate) fn substitute_bindings(bindings: &Bindings, expr: &Expr) -> Result<Expr> {
    Ok(match expr {
        Expr::Sym(_) => expr.clone(),
        Expr::Var(name) => bindings.get(name).unwrap_or(expr).clone(),
        Expr::Fun(head, args) => {
            let new_head = substitute_bindings(bindings, head)?;
            Expr::Fun(
                box new_head,
                args.iter()
                    .map(|arg| substitute_bindings(bindings, arg))
                    .collect::<Result<Vec<_>>>()?,
            )
        }
    })
}

pub(crate) fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    fn matches(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
        use Expr::*;
        match (pattern, value) {
            (Sym(name1), Sym(name2)) => name1 == name2,
            (Var(name), _) => {
                if let Some(existing) = bindings.get(name) {
                    existing == value
                } else {
                    bindings.insert(name.clone(), value.clone());
                    true
                }
            }
            (Fun(pat_name, pat_args), Expr::Fun(val_name, val_args)) => {
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
