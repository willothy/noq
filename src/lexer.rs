use std::iter::Peekable;

crate::token_kinds! {
    Ident,
    Comment,
    Invalid,
    Eof,
    String,
    Path,
    //Number,
    Rules = "rules",
    Commands = "commands",
    Reverse = "reverse",
    OpenParen = "(",
    CloseParen = ")",
    Comma = ",",
    Equals = "=",
    Semicolon = ";",
    -binops 2-
    Add = "+" 0,
    Sub = "-" 0,
    Mul = "*" 1,
    Div = "/" 1,
    Pow = "^" 2,
    -commands-
    Done = "done",
    Shape = "shape",
    Rule = "rule",
    Apply = "apply",
    Undo = "undo",
    Redo = "redo",
    Help = "help",
    Quit = "quit",
    Load = "load",
    Run = "run",
    Clear = "clear",
    Save = "save",
    Cd = "cd",
    Ls = "ls",
    Pwd = "pwd",
    -strategies-
    ApplyNth,
    ApplyAll = "all",
    ApplyFirst = "first",
    ApplyDeep = "deep",
    Check = "check"
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
}

impl Token {
    pub fn new(kind: TokenKind, text: String) -> Self {
        Self { kind, text }
    }
}

pub struct Lexer<T: Iterator<Item = char>> {
    pub chars: Peekable<T>,
    pub peeked: Option<Token>,
    prev: Option<Token>,
    pub exhausted: bool,
    pub text: String,
}

pub trait IsNumeric {
    fn is_numeric(&self) -> bool;
}

impl IsNumeric for String {
    fn is_numeric(&self) -> bool {
        self.chars().all(|c| c.is_numeric())
    }
}

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + crate::count!($($xs)*));
}

#[macro_export]
macro_rules! ignore {
    ($keep:ident, $rest:tt) => {
        $keep
    };
}

#[macro_export]
macro_rules! commands {
    () => {
        crate::lexer::COMMANDS
            .iter()
            .map(|x| format!("\"{}\"", x))
            .collect::<Vec<_>>()
            .join(", ")
    };
}

#[macro_export]
macro_rules! strategies {
    () => {
        crate::lexer::STRATEGIES
            .iter()
            .map(|x| format!("\"{}\"", x))
            .collect::<Vec<_>>()
            .join(", ")
    };
}

#[macro_export]
macro_rules! token_kinds {
    (
        $($kind:ident$(= $val:literal)?),+$(,)?
        -binops $max_prec:literal-
        $($op_kind:ident = $op_val:literal $op_prec:literal),+$(,)?
        -commands-
        $($cmd_kind:ident = $cmd_val:literal),+$(,)?
        -strategies-
        $($strat_kind:ident $(= $strat_val:literal)?),+$(,)?
    ) => {
        #[derive(Debug, PartialEq, Clone)]
        pub enum TokenKind {
            $($kind),+,
            Op(OpKind),
            Command(CommandKind),
            Strategy(StrategyKind)
        }

        #[derive(Debug, PartialEq, Clone)]
        pub enum OpKind {
            $($op_kind),+
        }

        #[allow(dead_code)]
        pub const MAX_PRECEDENCE: usize = $max_prec;

        impl OpKind {
            #[allow(dead_code)]
            pub fn precedence(&self) -> usize {
                use OpKind::*;
                match self {
                    $($op_kind => $op_prec),+
                }
            }
        }

        impl std::fmt::Display for OpKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use OpKind::*;
                match self {
                    $($op_kind => write!(f, $op_val),)+
                }
            }
        }

        #[derive(Debug, PartialEq, Clone)]
        pub enum CommandKind {
            $($cmd_kind),+
        }

        impl std::fmt::Display for CommandKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use CommandKind::*;
                match self {
                    $($cmd_kind => write!(f, $cmd_val),)+
                }
            }
        }

        #[derive(Debug, PartialEq, Clone)]
        pub enum StrategyKind {
            $($strat_kind),+
        }

        #[allow(dead_code)]
        pub const COMMANDS: [&str; crate::count!($($cmd_val)+)] = [
            $(
                casey::lower!($cmd_val)
            ),+
        ];

        #[allow(dead_code)]
        pub const BINOPS: [&str; crate::count!($($op_val)+)] = [
            $(
                casey::lower!($op_val)
            ),+
        ];

        #[allow(dead_code)]
        pub const STRATEGIES: [&str; crate::count!($($strat_kind)+)] = [
            $(
                casey::lower!(stringify!($strat_kind))
            ),+
        ];

        impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
            type Item = Token;

            fn next(&mut self) -> Option<Token> {
                if self.exhausted {
                    return None;
                } else {
                    Some(self.next_token())
                }
            }
        }


        impl<T: Iterator<Item = char>> Lexer<T> {
            pub fn new(chars: Peekable<T>) -> Self {
                Self {
                    chars,
                    peeked: None,
                    prev: None,
                    exhausted: false,
                    text: String::new(),
                }
            }

            pub fn peek(&mut self) -> &Token {
                let tok = self.next_token();
                self.peeked.insert(tok)
            }

            pub fn next_token(&mut self) -> Token {
                if let Some(token) = self.peeked.take() {
                    token
                } else {
                    self.next_token_impl()
                }
            }

            pub fn next_if<F: Fn(&Token) -> bool>(&mut self, pred: F) -> Option<Token> {
                let token = self.peek();
                if pred(&token) {
                    Some(self.next_token())
                } else {
                    None
                }
            }

            fn next_token_impl(&mut self) -> Token {
                let mut text = String::new();
                while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}
                if let Some(c) = self.chars.next() {
                    use TokenKind::*;
                    text.push(c);
                    match c.to_string().as_str() {
                        $($($val => Token::new($kind, text),)?)+
                        $($op_val => Token::new(Op(OpKind::$op_kind), text),)+
                        $($cmd_val => Token::new(Command(CommandKind::$cmd_kind), text),)+
                        $($($strat_val => Token::new(Strategy(StrategyKind::$strat_kind), text),)?)+
                        c => {
                            let c = c.chars().nth(0).unwrap();
                            if !c.is_alphanumeric() {
                                if c == '#' {
                                    while let Some(c) = self.chars.next_if(|x| *x != '\n') {
                                        text.push(c);
                                    }
                                    self.prev = Some(Token::new(Comment, text.clone()));
                                    return Token::new(Comment, text);
                                }
                                if c == '"' {
                                    while let Some(c) = self.chars.next_if(|x| *x != '"') {
                                        text.push(c);
                                    }
                                    self.prev = Some(Token::new(String, text[1..text.len() - 1].to_string()));
                                    return Token::new(String, text[1..text.len() - 1].to_string());
                                }
                                if c == '@' {
                                    while let Some(c) = self.chars.next_if(|x| !x.is_whitespace()) {
                                        text.push(c);
                                    }
                                    self.prev = Some(Token::new(Path, text[1..text.len()].to_string()));
                                    return Token::new(Path, text[1..text.len()].to_string());
                                }
                                match c {
                                    '.' => {}
                                    _ => {
                                        self.prev = Some(Token::new(Invalid, c.to_string()));
                                        return Token {
                                            kind: Invalid,
                                            text: c.to_string(),
                                        };
                                    }
                                }

                            }

                            while let Some(c) = self
                                .chars
                                .next_if(|x| (x.is_alphanumeric() || *x == '.' || *x == '_' || *x == '\\') && !x.is_whitespace())
                            {
                                text.push(c);
                            }


                            if text.is_numeric() {
                                if let Some(prev) = &self.prev {
                                    if prev.kind == Command(CommandKind::Apply) {
                                        return Token::new(Strategy(StrategyKind::ApplyNth), text);
                                    }
                                }
                                //return Token::new(Number, text);
                            }

                            let token = match text.as_str() {
                                $($($val => Token::new($kind, text),)?)+
                                $($op_val => Token::new(Op(OpKind::$op_kind), text),)+
                                $($cmd_val => Token::new(Command(CommandKind::$cmd_kind), text),)+
                                $($($strat_val => Token::new(Strategy(StrategyKind::$strat_kind), text),)?)+
                                _ => Token::new(Ident, text),
                            };
                            self.prev = Some(token.clone());
                            token
                        }
                    }
                } else {
                    self.exhausted = true;
                    Token {
                        kind: TokenKind::Eof,
                        text: String::new(),
                    }
                }
            }
        }
    };
}
