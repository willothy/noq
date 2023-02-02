use crate::{expr::Expr, lexer::Constraint, rule::Bindings};

pub(crate) fn substitute_bindings(bindings: &Bindings, expr: &Expr) -> Expr {
    match expr {
        Expr::Sym(_) => expr.clone(),
        Expr::Num(_) => expr.clone(),
        Expr::Str(_) => expr.clone(),
        Expr::Var(name, _) => bindings.get(name).unwrap_or(expr).clone(),
        Expr::Op(op, l, r) => Expr::Op(
            op.clone(),
            box substitute_bindings(bindings, l),
            box substitute_bindings(bindings, r),
        ),
        Expr::Fun(head, body) => Expr::Fun(
            box substitute_bindings(bindings, head),
            box substitute_bindings(bindings, body),
        ),
        Expr::List(elements) => Expr::List(
            elements
                .iter()
                .map(|el| substitute_bindings(bindings, el))
                .collect(),
        ),
    }
}

pub(crate) fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    fn match_impl(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
        use Expr::*;
        match (pattern, value) {
            (Num(n1), Num(n2)) => n1 == n2,
            (Str(s1), Str(s2)) => s1 == s2,
            (Sym(name1), Sym(name2)) => name1 == name2,
            (Var(name, constraint), other) => {
                use Constraint::*;
                let matches_constraint = match (constraint, &other) {
                    (Sym, Expr::Sym(_))
                    | (Num, Expr::Num(_))
                    | (Str, Expr::Str(_))
                    | (List, Expr::List(_))
                    | (Fun, Expr::Fun(_, _))
                    | (None, _) => true,
                    (_, Expr::Var(_, other_constraint)) => {
                        constraint == other_constraint || *other_constraint == Constraint::None
                    }
                    _ => false,
                };
                if name == "_" {
                    matches_constraint
                } else if let Some(existing) = bindings.get(name) {
                    matches_constraint && existing == value
                } else {
                    if matches_constraint {
                        bindings.insert(name.clone(), value.clone());
                    }
                    matches_constraint
                }
            }
            (Op(opl, l1, r1), Op(opr, l2, r2)) => {
                opl == opr && match_impl(l1, l2, bindings) && match_impl(r1, r2, bindings)
            }
            (Fun(pat_name, pat_body), Expr::Fun(val_name, val_body)) => {
                match_impl(pat_name, val_name, bindings) && match_impl(pat_body, val_body, bindings)
            }
            (List(pat_elements), List(val_elements)) => {
                if pat_elements.len() != val_elements.len() {
                    false
                } else {
                    pat_elements
                        .iter()
                        .zip(val_elements.iter())
                        .all(|(pat, val)| match_impl(pat, val, bindings))
                }
            }
            _ => false,
        }
    }

    let mut bindings = Bindings::new();

    if match_impl(pattern, value, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

pub(crate) fn collect_subexprs<'a>(pattern: &'a Expr, expr: &'a Expr) -> Vec<&'a Expr> {
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

pub(crate) fn collect_sub_constexprs<'a>(expr: &'a Expr) -> Vec<&'a Expr> {
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
