use std::{collections::HashMap, fmt::Display};

use crate::{
    expr::Expr,
    lexer::{Constraint, Lexer, TokenKind},
};
use anyhow::Result;
use thiserror::Error;

pub(crate) type Bindings = HashMap<String, Expr>;

#[derive(Debug, Error)]
#[error("Match error: {0}")]
pub struct MatchError(String);

#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub head: Expr,
    pub body: Expr,
}

impl Rule {
    pub fn parse(mut lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self> {
        let head = Expr::parse(&mut lexer)?;
        lexer
            .next_if(|t| t.kind == TokenKind::Equals)
            .ok_or_else(|| MatchError(format!("Expected '=', got {}", lexer.next_token())))?;
        let body = Expr::parse(&mut lexer)?;
        Ok(Self { head, body })
    }

    pub fn reverse(&self) -> Self {
        Self {
            head: self.body.clone(),
            body: self.head.clone(),
        }
    }
}

impl Rule {
    pub fn apply(&self, expr: &Expr, strategy: &mut impl Strategy) -> Expr {
        fn apply_to_subexprs(
            rule: &Rule,
            expr: &Expr,
            strategy: &mut impl Strategy,
        ) -> (Expr, bool) {
            use Expr::*;
            match expr {
                Num(_) | Str(_) | Sym(_) | Var(_, _) => (expr.clone(), false),
                Op(op, lhs, rhs) => {
                    let (new_lhs, halt) = apply_impl(rule, lhs, strategy);
                    if halt {
                        return (Op(op.clone(), box new_lhs, rhs.clone()), true);
                    }
                    let (new_rhs, halt) = apply_impl(rule, rhs, strategy);
                    (Op(op.clone(), box new_lhs, box new_rhs), halt)
                }
                Fun(head, body) => {
                    let (new_head, halt) = apply_impl(rule, head, strategy);
                    if halt {
                        return (Fun(box new_head, body.clone()), true);
                    }
                    let new_body = apply_impl(rule, body, strategy).0;
                    (Fun(box new_head, box new_body), false)
                }
                List(elements) => {
                    let mut new_elements = vec![];
                    let mut halt_elements = false;
                    for element in elements {
                        if halt_elements {
                            new_elements.push(element.clone());
                        } else {
                            let (arg, arg_halt) = apply_impl(rule, element, strategy);
                            new_elements.push(arg);
                            halt_elements = arg_halt;
                        }
                    }
                    (List(new_elements), false)
                }
            }
        }

        fn apply_impl(rule: &Rule, expr: &Expr, strategy: &mut impl Strategy) -> (Expr, bool) {
            if let Some(bindings) = pattern_match(&rule.head, expr) {
                let resolution = strategy.matched();
                let new_expr = match resolution.action {
                    Action::Apply => substitute_bindings(&bindings, &rule.body),
                    Action::Skip => expr.clone(),
                    Action::Check => {
                        if let Some(matches) = strategy.matches() {
                            matches
                                .push((expr.clone(), substitute_bindings(&bindings, &rule.body)));
                        }
                        expr.clone()
                    }
                };
                match resolution.state {
                    State::Bail => (new_expr, false),
                    State::Halt => (new_expr, true),
                    State::Cont => apply_to_subexprs(rule, &new_expr, strategy),
                }
            } else {
                apply_to_subexprs(rule, expr, strategy)
            }
        }
        apply_impl(self, expr, strategy).0
    }
}

impl TryFrom<&str> for Rule {
    type Error = anyhow::Error;
    fn try_from(s: &str) -> Result<Self> {
        Rule::parse(&mut crate::lexer::Lexer::new(s.chars().peekable()))
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.head, self.body)
    }
}

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
                if pat_elements.len() == 1
                /* && &pat_elements[0] == &Expr::Var("_".to_string(), ) */
                {
                    true
                } else if pat_elements.len() != val_elements.len() {
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

pub enum Action {
    #[allow(dead_code)]
    Skip,
    Check,
    Apply,
}

pub enum State {
    /// Stop the current recursion branch and try other braunches
    Bail,
    /// Continue applying the rule to the result of the application
    Cont,
    /// Completely stop the application process
    Halt,
}

pub struct Resolution {
    pub action: Action,
    pub state: State,
}

pub(crate) trait Strategy {
    fn matched(&mut self) -> Resolution;
    fn matches(&mut self) -> Option<&mut Vec<(Expr, Expr)>> {
        None
    }
}

// Strategies
pub struct ApplyCheck {
    matches: Vec<(Expr, Expr)>,
}
pub struct ApplyAll;
pub struct ApplyFirst;
pub struct ApplyDeep;
pub struct ApplyNth {
    current: usize,
    target: usize,
}

impl ApplyCheck {
    pub fn new() -> Self {
        Self { matches: vec![] }
    }
}

impl Strategy for ApplyCheck {
    fn matched(&mut self) -> Resolution {
        Resolution {
            action: Action::Check,
            state: State::Cont,
        }
    }

    fn matches(&mut self) -> Option<&mut Vec<(Expr, Expr)>> {
        Some(&mut self.matches)
    }
}

impl Strategy for ApplyAll {
    fn matched(&mut self) -> Resolution {
        Resolution {
            action: Action::Apply,
            state: State::Bail,
        }
    }
}

impl Strategy for ApplyFirst {
    fn matched(&mut self) -> Resolution {
        Resolution {
            action: Action::Apply,
            state: State::Halt,
        }
    }
}

impl Strategy for ApplyDeep {
    fn matched(&mut self) -> Resolution {
        Resolution {
            action: Action::Apply,
            state: State::Cont,
        }
    }
}

impl ApplyNth {
    pub fn new(target: usize) -> Self {
        Self { current: 0, target }
    }
}

impl Strategy for ApplyNth {
    fn matched(&mut self) -> Resolution {
        if self.current == self.target {
            Resolution {
                action: Action::Apply,
                state: State::Halt,
            }
        } else if self.current > self.target {
            Resolution {
                action: Action::Skip,
                state: State::Halt,
            }
        } else {
            self.current += 1;
            Resolution {
                action: Action::Skip,
                state: State::Cont,
            }
        }
    }
}
