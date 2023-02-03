# Woq
### My version of Tsoding's Noq expression transformer, with some of my own added features.
<br>

## Features and Plans

- [X] Pattern matching to replace expressions based on rules
- [X] List expressions
    - [X] Basic list usage
    - [X] Repeating expressions in lists
    - [X] List conversion to binary op with separators
    - [ ] Retrieving iteration index with `@`
- [X] Unary expression
    - [X] `+` and `-` unary operators
    - [X] `+` and `-` unary operators with list conversion
- [X] Variable type constraints
    - [X] `#` for number, `%` for string, `$` for symbol, `!` for function, `[]` for list
    - [X] Use in rules to constrain the variable type they match
- [X] Constant expression folding / evaluation
    - [X] `eval` intrinsic
    - [ ] Evaluate list exprs (dot product, vec add, etc)
- [ ] LaTeX or other rendered math expr output (maybe)
- [ ] Neovim integration (WIP)


## Examples:

### `Basic Expressions`:
    Number: Positive and negative values allowed
    String: Text enclosed in double quotes - escapes not yet supported
    Symbol: identifier starting with a lowercase letter or underscore, matched literally
    Variable: identifier starting with an uppercase letter, bound to a value when matching

    X + 5 => Variable + Number
    "Hello, world!" => String
    foo => Symbol

### `List Expressions`:
    (1, 2, 3, 4)

    Repeating patterns:
    (N, ..) => matches (N, N, N, etc.)

    Repeating pattern with conversion to binop:
    (N, +..) => N + N + N + N ...

    Vector Add:
    (1, 2, 3) + (4, 5, 6) => List + List

### `Function Expressions`:
    f(X, Y) => Function(Var, Var)
    f(X, Y)(G, (H, I)) => Function(Var, Var)(Var, List)

### `Rules`:
#### Definition:
<code>name &#58;&#58; pattern &#61; replacement</code><br>
Add single number to vector:
<code>vec_add_one &#58;&#58; (A, ..) + N = (A + 1, ..)</code>
<code>(1, 2, 3) + 5 => (1 + 5, 2 + 5, 3 + 5)</code><br>
Vector add:
<code>vec_add &#58;&#58; &#40;A, ..) + (B, ..) = (A + B, ..)</code><br>
Vector dot product:
    <code>dot &#58;&#58; (A, ..) * (B, ..) = (A * B, +..)</code>
    <code style="margin-left: 22px;">=> (A0 * B0) + (A1 * B1) + (A2 * B2) + ...</code>
</code>

#### Application:
<code>rule | strategy</code><br>
<code>dot | 0</code> applies the vector dot product rule to its first match in current shape
<code>vec_add | all</code> applies the vec_add rule to all matches in current shape

#### Checking:
Using the intrinsic `check` strategy, you can print all matches for a given rule. You use check the same way as a rule application strategy:
<code>dot | check</code> checks for matches of the dot rule in the current shape
<code>vec_add | check </code> checks for matches of the vec_add rule in the current shape

### `Variable Constraints`:
    X# => Number
    X% => String
    X$ => Symbol
    X! => Function
    X[] => List

    Add single number to vector (with type constraints):
    vec_add_one :: (A#, ..) + N# = (A + 1, ..)

### `Constant Folding`:
    eval | <strategy>

    eval is an intrinsic which matches and evaluates constant expressions.

    // example uses:
    eval | 0
    eval | check

[//]: # (Colon: &#58; Open, close paren: &#40;, &#41;)
[//]: # (equals: &#61;)
[//]: # (sub: &#115;)
[//]: # (sup: &#115;)
[//]: # (plus: &#43;)
[//]: # (star: &#42;)
[//]: # (tilde: &#126;)
[//]: # (underscore: &#95;)
[//]: # (comma: &#44;)
[//]: # (period: &#46;)
[//]: # (pipe: &#124;)
[//]: # (caret: &#94;)
