#![cfg(test)]
use std::collections::HashMap;

use crate::{expr::Expr, matching::pattern_match, rule::*};

#[test]
fn rule_apply_all() {
    let swap = Rule::try_from("swap((A, B)) = (B, A)").unwrap();

    let input = Expr::try_from("foo(swap((f(a), g(b))), swap((q(c), z(d))))").unwrap();

    let expected = Expr::try_from("foo((g(b), f(a)), (z(d), q(c)))").unwrap();

    let actual = swap.apply(&input, &mut ApplyAll);

    assert_eq!(actual, expected);
}

#[test]
fn matching() {
    let swap = Rule::try_from("swap(pair(A, B)) = pair(B, A)").unwrap();

    let pattern = swap.head.clone();
    let value = Expr::try_from("swap(pair(f(c), g(d)))").unwrap();

    let expected: HashMap<&str, _> = HashMap::from_iter(vec![
        ("A", Expr::try_from("f(c)").unwrap()),
        ("B", Expr::try_from("g(d)").unwrap()),
    ]);

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
