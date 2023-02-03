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
- [X] Constant expression folding / evaluation
    - [X] `eval` intrinsic
    - [ ] Evaluate list exprs (dot product, vec add, etc)
- [ ] LaTeX or other rendered math expr output (maybe)
- [ ] Neovim integration (WIP)


## Examples:

### Basic Expressions: 
    Number: Unsigned integers only (for now)
    String: Text enclosed in double quotes - escapes not yet supported
    Symbol: identifier starting with a lowercase letter or underscore, matched literally
    Variable: identifier starting with an uppercase letter, bound to a value when matching

    X + 5 => Variable + Number
    "Hello, world!" => String
    foo => Symbol

#### List Expressions:
    (1, 2, 3, 4)

    Repeating patterns:
    (N, ..) => matches (N, N, N, etc.)

    Repeating pattern with conversion to binop:
    (N, +..) => N + N + N + N ...

    Vector Add:
    (1, 2, 3) + (4, 5, 6) => List + List

#### Function Expressions:
    f(X, Y) => Function(Var, Var)
    f(X, Y)(G, (H, I)) => Function(Var, Var)(Var, List)

### Rules:<br>
<code>name &#58;&#58; pattern &#61; replacement</code><br>
Vector add:
<code>vec_add &#58;&#58; &#40;A, ..) + (B, ..) = (A<sub>n</sub> + B~n~, ..)</code><br>
Vector dot product:
    <code>dot &#58;&#58; (A, ..) * (B, ..) = (A * B, +..)</code>
    <code style="margin-left: 22px;">=> (A<sub>0</sub> * B<sub>0</sub>) + (A<sub>1</sub> * B<sub>1</sub>) + (A<sub>2</sub> * B<sub>2</sub>) + ...</code>
</code>

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
