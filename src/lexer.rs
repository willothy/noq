use std::{collections::VecDeque, fmt::Display, iter::Peekable};

crate::token_kinds! {
    Ident,
    Comment,
    Invalid,
    Eof,
    String,
    UnclosedStr,
    Path,
    Number,
    Rules = "rules",
    Commands = "commands",
    Reverse = "reverse",
    Colon,
    DoubleColon = "::",
    OpenParen = "(",
    CloseParen = ")",
    Dot,
    DoubleDot = "..",
    Comma = ",",
    Equals = "=",
    Semicolon = ";",
    OpenBrace = "{",
    CloseBrace = "}",
    Bar = "|",
    Bang = "!",
    -binops 2-
    Add = "+" 0,
    Sub = "-" 0,
    Mul = "*" 1,
    Div = "/" 1,
    Pow = "^" 2,
    -commands-
    Done = "done",
    Undo = "undo",
    Redo = "redo",
    Help = "help",
    Quit = "quit",
    Use = "use",
    Run = "run",
    Clear = "clear",
    Save = "save",
    Cd = "cd",
    Ls = "ls",
    Pwd = "pwd",
    -strategies-
    ApplyAll = "all",
    ApplyFirst = "first",
    ApplyDeep = "deep",
    Check = "check",
    ApplyNth
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Loc,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Invalid => write!(f, "Invalid ({})", self.text),
            TokenKind::Comment => write!(f, "Comment ({})", self.text),
            TokenKind::Strategy(_) => write!(f, "Strategy ({})", self.text),
            TokenKind::Op(_) => write!(f, "Op ({})", self.text),
            TokenKind::Command(_) => write!(f, "Command ({})", self.text),
            _ => write!(f, "'{}'", self.text),
        }
    }
}

pub trait StringUnwrap {
    fn unwrap_string(self) -> String;
}

impl<T> StringUnwrap for Option<T>
where
    T: Display,
{
    fn unwrap_string(self) -> String {
        match self {
            Some(t) => format!("{}", t),
            None => "EOF".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loc {
    pub file: Option<String>,
    pub line: usize,
    pub col: usize,
    pub offset: usize,
}

impl Loc {
    pub fn prev_col(&self) -> Self {
        Self {
            file: self.file.clone(),
            line: self.line,
            col: if self.col > 1 { self.col - 1 } else { 1 },
            offset: if self.offset > 0 { self.offset - 1 } else { 0 },
        }
    }
}

impl Default for Loc {
    fn default() -> Self {
        Self {
            file: None,
            line: 1,
            col: 1,
            offset: 0,
        }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(file) = &self.file {
            write!(f, "{}:{}:{}", file, self.line, self.col)
        } else {
            write!(f, "{}:{}", self.line, self.col)
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, text: String, loc: Loc) -> Self {
        Self { kind, text, loc }
    }
}

pub struct Lexer<T: Iterator<Item = char>> {
    pub chars: Peekable<T>,
    pub peeked: VecDeque<Token>,
    prev: Option<Token>,
    pub exhausted: bool,
    pub file_name: Option<String>,
    pub current_line: usize,
    pub current_col: usize,
    pub current_offset: usize,
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

        #[allow(dead_code)]
        pub const KEYWORDS: [&str; 3] = [
            "rules",
            "commands",
            "reverse"
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
                    peeked: VecDeque::new(),
                    prev: None,
                    exhausted: false,
                    file_name: None,
                    current_line: 1,
                    current_col: 1,
                    current_offset: 0
                }
            }

            pub fn current_loc(&self) -> Loc {
                Loc {
                    line: self.current_line,
                    col: self.current_col,
                    file: self.file_name.clone(),
                    offset: self.current_offset,
                }
            }

            pub fn with_file_name(mut self, file_name: String) -> Self {
                self.file_name = Some(file_name);
                self
            }

            pub fn peek(&mut self) -> &Token {
                let tok = self.next_token();
                self.peeked.push_front(tok);
                self.peeked.front().unwrap()
            }

            pub fn peek_next(&mut self) -> &Token {
                let tok = self.next_token_impl();
                self.peeked.push_front(tok);
                self.peeked.front().unwrap()
            }

            pub fn catchup(&mut self) {
                self.peeked.clear();
            }

            pub fn next_token(&mut self) -> Token {
                if let Some(token) = self.peeked.pop_back() {
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
                while let Some(whitespace) = self.chars.next_if(|x| x.is_whitespace()) {
                    if whitespace == '\n' {
                        self.current_line += 1;
                        self.current_col = 1;
                    } else {
                        self.current_col += 1;
                    }
                    self.current_offset += 1;
                }

                if let Some(c) = self.chars.next() {
                    let loc = self.current_loc();
                    use TokenKind::*;
                    text.push(c);
                    self.current_col += 1;
                    self.current_offset += 1;
                    match c.to_string().as_str() {
                        $($($val => {
                            self.prev = Some(Token::new($kind, text.clone(), loc.clone()));
                            Token::new($kind, text, loc)
                        },)?)+
                        $($op_val => {
                            self.prev = Some(Token::new(Op(OpKind::$op_kind), text.clone(), loc.clone()));
                            Token::new(Op(OpKind::$op_kind), text, loc)
                        },)+
                        $($cmd_val => {
                            self.prev = Some(Token::new(Command(CommandKind::$cmd_kind), text.clone(), loc.clone()));
                            Token::new(Command(CommandKind::$cmd_kind), text, loc)
                        },)+
                        $($($strat_val => {
                            self.prev = Some(Token::new(Strategy(StrategyKind::$strat_kind), text.clone(), loc.clone()));
                            Token::new(Strategy(StrategyKind::$strat_kind), text, loc)
                        },)?)+
                        c => {
                            let c = c.chars().nth(0).unwrap();
                            if !c.is_alphanumeric() && !c.is_whitespace() {
                                if c == '#' {
                                    while let Some(c) = self.chars.next_if(|x| *x != '\n') {
                                        self.current_offset += 1;
                                        self.current_col += 1;
                                        text.push(c);
                                    }
                                    self.prev = Some(Token::new(Comment, text.clone(), loc.clone()));
                                    return Token::new(Comment, text, loc);
                                }
                                if c == '"' {
                                    text.pop();
                                    while let Some(c) = self.chars.next_if(|x| *x != '"') {
                                        self.current_offset += 1;
                                        self.current_col += 1;
                                        text.push(c);
                                    }
                                    let Some('"') = self.chars.next() else {
                                        self.prev = Some(Token::new(UnclosedStr, text.clone(), loc.clone()));
                                        return Token::new(UnclosedStr, text, loc);
                                    };
                                    self.prev = Some(Token::new(String, text.to_string(), loc.clone()));
                                    return Token::new(String, text.to_string(), loc);
                                }
                                if c == '@' {
                                    while let Some(c) = self.chars.next_if(|x| !x.is_whitespace()) {
                                        self.current_offset += 1;
                                        self.current_col += 1;
                                        text.push(c);
                                    }
                                    self.prev = Some(Token::new(Path, text[1..text.len()].to_string(), loc.clone()));
                                    return Token::new(Path, text[1..text.len()].to_string(), loc);
                                }
                                match c {
                                    '.' => {
                                        if self.chars.next_if(|x| *x == '.').is_some() {
                                            self.current_offset += 1;
                                            self.current_col += 1;
                                            text.push('.');
                                            let tok = Token::new(DoubleDot, text, loc);
                                            self.prev = Some(tok.clone());
                                            return tok;
                                        } else {
                                            let tok = Token::new(Dot, text, loc);
                                            self.prev = Some(tok.clone());
                                            return tok;
                                        }
                                    }
                                    ':' => {
                                        if let Some(c) = self.chars.next_if(|x| *x == ':') {
                                            self.current_offset += 1;
                                            self.current_col += 1;
                                            text.push(c);
                                            let tok = Token::new(DoubleColon, text, loc);
                                            self.prev = Some(tok.clone());
                                            return tok;
                                        } else {
                                            let tok = Token::new(Colon, text, loc);
                                            self.prev = Some(tok.clone());
                                            return tok;
                                        }
                                    }
                                    _ => {
                                        self.prev = Some(Token::new(Invalid, c.to_string(), loc.clone()));
                                        return Token {
                                            kind: Invalid,
                                            text: c.to_string(),
                                            loc
                                        };
                                    }
                                }

                            }

                            while let Some(c) = self
                                .chars
                                .next_if(|x| (x.is_alphanumeric() || *x == '.' || *x == '_' || *x == '\\') && !x.is_whitespace() && *x != '\n')
                            {
                                text.push(c);
                                self.current_col += 1;
                                self.current_offset += 1;
                            }


                            if text.is_numeric() {
                                if let Some(prev) = &self.prev {
                                    if prev.kind == Bar || prev.kind == Bang {
                                        return Token::new(Strategy(StrategyKind::ApplyNth), text, loc);
                                    }
                                }
                                return Token::new(Number, text, loc);
                            }

                            let token = match text.as_str() {
                                $($($val => Token::new($kind, text, loc),)?)+
                                $($op_val => Token::new(Op(OpKind::$op_kind), text, loc),)+
                                $($cmd_val => Token::new(Command(CommandKind::$cmd_kind), text, loc),)+
                                $($($strat_val => Token::new(Strategy(StrategyKind::$strat_kind), text, loc),)?)+
                                _ => Token::new(Ident, text, loc),
                            };
                            self.prev = Some(token.clone());
                            token
                        }
                    }
                } else {
                    let loc = self.current_loc();
                    self.exhausted = true;
                    Token {
                        kind: TokenKind::Eof,
                        text: String::new(),
                        loc
                    }
                }
            }
        }
    };
}
