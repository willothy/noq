#![cfg(test)]
use std::collections::HashMap;

use crate::bindings::*;
use crate::rule::Expr;
use crate::rule::Rule;
use crate::*;

#[test]
fn rule_apply_all() {
    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };

    let input = expr!(foo(swap(pair(f(a), g(b))), swap(pair(q(c), z(d)))));

    let expected = expr!(foo(pair(g(b), f(a)), pair(z(d), q(c))));

    let actual = swap.apply_all(&input);

    assert_eq!(actual, expected);
}

#[test]
fn matching() {
    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };

    // Pattern: swap(pair(a, b))
    let pattern = swap.head.clone();
    // swap(pair(f(c), g(d)))
    let value = expr!(swap(pair(f(c), g(d))));
    let expected: HashMap<&str, Expr> =
        HashMap::from_iter(vec![("a", expr!(f(c))), ("b", expr!(g(d)))]);

    if let Some(bindings) = pattern_match(&pattern, &value) {
        eprintln!("MATCH:");
        for (name, value) in bindings.iter() {
            let exp_value = expected.get(name.as_str()).unwrap();
            eprintln!("Expected: {} = {}", name, exp_value);
            eprintln!("Found:    {} = {}", name, value);
            assert!(value == exp_value);
        }
    } else {
        eprintln!("{}", pattern);
        eprintln!("{}", value);
        panic!("No match");
    }
}
