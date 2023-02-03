use std::collections::{HashMap, VecDeque};

use crate::{
    expr::{Expr, Repeat},
    lexer::Constraint,
};

pub(crate) fn substitute_bindings(
    bindings: &mut Bindings,
    expr: &Expr,
    in_repeat: bool,
    repeat_exhausted: &mut bool,
) -> Expr {
    match expr {
        Expr::Sym(_) => expr.clone(),
        Expr::Num(_) => expr.clone(),
        Expr::Str(_) => expr.clone(),
        Expr::Var(name, _) => {
            if let Some(binding) = bindings.get_mut(name) {
                match binding {
                    Binding::Var(var) => {
                        if in_repeat {
                            //*repeat_exhausted = true;
                            /* match bindings.remove(name).unwrap() {
                                Binding::Var(var) => var,
                                _ => unreachable!(),
                            } */
                        } else {
                        }
                        var.clone()
                    }
                    Binding::List(list) => {
                        if list.is_empty() {
                            panic!("Tried to use an empty list as a variable");
                        }
                        let res = list.pop_front().unwrap();
                        if list.is_empty() {
                            *repeat_exhausted = true;
                            //bindings.remove(name);
                        }
                        res
                    }
                }
            } else {
                /* if in_repeat {
                    *repeat_exhausted = true;
                } */
                expr.clone()
            }
        }
        Expr::Op(op, l, r) => Expr::Op(
            op.clone(),
            box substitute_bindings(bindings, l, in_repeat, repeat_exhausted),
            box substitute_bindings(bindings, r, in_repeat, repeat_exhausted),
        ),
        Expr::Fun(head, body) => Expr::Fun(
            box substitute_bindings(bindings, head, in_repeat, repeat_exhausted),
            box substitute_bindings(bindings, body, in_repeat, repeat_exhausted),
        ),
        Expr::List(elements, repeat) => {
            // no resize
            match repeat {
                Repeat::None => Expr::List(
                    elements
                        .iter()
                        .map(|el| substitute_bindings(bindings, el, in_repeat, repeat_exhausted))
                        .collect(),
                    repeat.clone(),
                ),
                Repeat::ZeroOrMore(None) => {
                    let mut new_elements = Vec::new();
                    let mut repeat_bindings = bindings.clone();
                    let mut depth = 0;
                    let mut exhausted = false;
                    while !exhausted {
                        for element in elements {
                            new_elements.push(substitute_bindings(
                                &mut repeat_bindings,
                                element,
                                true,
                                &mut exhausted,
                            ));
                        }
                        depth += 1;
                        if depth > 100 {
                            panic!("Infinite loop detected");
                        }
                    }

                    Expr::List(new_elements, repeat.clone())
                }
                Repeat::ZeroOrMore(Some(op)) => {
                    let mut new_elements = VecDeque::new();
                    //let mut repeat_bindings = bindings.clone();
                    let mut depth = 0;
                    let mut exhausted = false;
                    while !exhausted {
                        for element in elements {
                            new_elements.push_back(substitute_bindings(
                                bindings,
                                element,
                                true,
                                &mut exhausted,
                            ));
                        }
                        depth += 1;
                        if depth > 100 {
                            panic!("Infinite loop detected");
                        }
                    }

                    while new_elements.len() > 1 {
                        let combined = new_elements.pop_front().unwrap();
                        let second = new_elements.pop_front().unwrap();
                        let new = Expr::Op(op.clone(), box combined, box second);
                        new_elements.push_front(new);
                    }

                    new_elements.pop_front().unwrap()
                }
            }
        }
    }
}

pub(crate) type Bindings = HashMap<String, Binding>;

#[derive(Debug, Clone)]
pub(crate) enum Binding {
    Var(Expr),
    List(VecDeque<Expr>),
}

impl Binding {
    pub fn assert_var(&self) -> &Expr {
        match self {
            Binding::Var(expr) => expr,
            Binding::List(_) => panic!("Expected a variable, got a list"),
        }
    }
}

pub(crate) fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    fn match_impl(pattern: &Expr, value: &Expr, bindings: &mut Bindings, in_repeat: bool) -> bool {
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
                    | (List, Expr::List(_, _))
                    | (Fun, Expr::Fun(_, _))
                    | (None, _) => true,
                    (_, Expr::Var(_, other_constraint)) => {
                        constraint == other_constraint || *other_constraint == Constraint::None
                    }
                    _ => false,
                };
                if name == "_" {
                    matches_constraint
                } else if !in_repeat && let Some(existing) = bindings.get(name) {
                    matches_constraint
                        && existing.assert_var() == value
                } else {
                    if matches_constraint {
                        if in_repeat {
                            if let Some(Binding::List(list)) = bindings.get_mut(name) {
                                list.push_back(value.clone());
                            } else {
                                bindings.insert(name.clone(), Binding::List(VecDeque::from([value.clone()])));
                            }
                        } else {
                            bindings.insert(name.clone(), Binding::Var(value.clone()));
                        }
                    }
                    matches_constraint
                }
            }
            (Op(opl, l1, r1), Op(opr, l2, r2)) => {
                opl == opr
                    && match_impl(l1, l2, bindings, in_repeat)
                    && match_impl(r1, r2, bindings, in_repeat)
            }
            (Fun(pat_name, pat_body), Expr::Fun(val_name, val_body)) => {
                match_impl(pat_name, val_name, bindings, in_repeat)
                    && match_impl(pat_body, val_body, bindings, in_repeat)
            }
            (List(pat_elements, pat_repeat), List(val_elements, _)) => {
                let pat_len = pat_elements.len();
                let val_len = val_elements.len();

                if pat_len != val_len && *pat_repeat == Repeat::None {
                    false
                } else {
                    match pat_repeat {
                        Repeat::None => pat_elements
                            .iter()
                            .zip(val_elements.iter())
                            .all(|(pat, val)| match_impl(pat, val, bindings, in_repeat)),
                        Repeat::ZeroOrMore(_) => {
                            if val_len % pat_len != 0 {
                                false
                            } else {
                                for i in 0..val_len {
                                    if !match_impl(
                                        &pat_elements[i % pat_len],
                                        &val_elements[i],
                                        bindings,
                                        true,
                                    ) {
                                        return false;
                                    }
                                }
                                true
                            }
                        }
                    }
                }
            }
            _ => false,
        }
    }

    let mut bindings = Bindings::new();

    if match_impl(pattern, value, &mut bindings, false) {
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
            Expr::List(elements, _) => {
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
            Expr::List(elements, _) => {
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
