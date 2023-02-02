use std::{collections::HashMap, fmt::Display};

use crate::{
    err,
    error::common::*,
    error::ParseError::UnexpectedToken,
    expr::Expr,
    lexer::{Lexer, TokenKind},
    matching::{pattern_match, substitute_bindings},
};

pub(crate) type Bindings = HashMap<String, Expr>;

#[derive(Debug, Clone)]
pub(crate) struct Rule {
    pub(crate) head: Expr,
    pub(crate) body: Expr,
}

impl Rule {
    pub(crate) fn parse(mut lexer: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self> {
        let head = Expr::parse(&mut lexer)?;
        if lexer.next_if(|t| t.kind == TokenKind::Equals).is_none() {
            let invalid = lexer.next_token();
            return err!(Parse UnexpectedToken(invalid.text), invalid.loc)
                .with_message("Expected '='");
        }
        let body = Expr::parse(&mut lexer)?;
        Ok(Self { head, body })
    }

    pub(crate) fn reverse(&self) -> Self {
        Self {
            head: self.body.clone(),
            body: self.head.clone(),
        }
    }
}

impl Rule {
    pub(crate) fn apply(&self, expr: &Expr, strategy: &mut impl Strategy) -> Expr {
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
    type Error = Error;
    fn try_from(s: &str) -> Result<Self> {
        Rule::parse(&mut crate::lexer::Lexer::new(s.chars().peekable()))
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.head, self.body)
    }
}

pub(crate) enum Action {
    #[allow(dead_code)]
    Skip,
    Check,
    Apply,
}

pub(crate) enum State {
    /// Stop the current recursion branch and try other braunches
    Bail,
    /// Continue applying the rule to the result of the application
    Cont,
    /// Completely stop the application process
    Halt,
}

pub(crate) struct Resolution {
    pub(crate) action: Action,
    pub(crate) state: State,
}

pub(crate) trait Strategy {
    fn matched(&mut self) -> Resolution;
    fn matches(&mut self) -> Option<&mut Vec<(Expr, Expr)>> {
        None
    }
}

// Strategies
pub(crate) struct ApplyCheck {
    matches: Vec<(Expr, Expr)>,
}
pub(crate) struct ApplyAll;
pub(crate) struct ApplyFirst;
pub(crate) struct ApplyDeep;
pub(crate) struct ApplyNth {
    current: usize,
    target: usize,
}

impl ApplyCheck {
    pub(crate) fn new() -> Self {
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
    pub(crate) fn new(target: usize) -> Self {
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
