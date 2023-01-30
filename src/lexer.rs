use std::{iter::Peekable, str::Chars};

crate::token_kinds! {
    Sym,
    Comment,
    OpenParen = "(",
    CloseParen = ")",
    Comma = ",",
    Equals = "=",
    Semicolon = ";",
    All = "all",
    Done = "done" command,
    Shape = "shape" command,
    Rule = "rule" command,
    Apply = "apply" command,
    Undo = "undo" command,
    Redo = "redo" command,
    Help = "help" command,
    Quit = "quit" command,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
}

impl Token {
    pub fn new(kind: TokenKind, text: String) -> Self {
        Self { kind, text }
    }
}

pub struct Lexer<'a> {
    pub chars: Peekable<Chars<'a>>,
    pub text: String,
}

impl<'a> Lexer<'a> {
    pub fn from_iter(chars: Peekable<Chars<'a>>) -> Self {
        Self {
            chars,
            text: String::new(),
        }
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(str: &'a str) -> Self {
        Lexer::from_iter(str.chars().peekable())
    }
}

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + crate::count!($($xs)*));
}

#[macro_export]
macro_rules! ignore {
    ($first:ident, $last:expr) => {
        $last
    };
}

#[macro_export]
macro_rules! commands {
    () => {
        crate::lexer::COMMANDS.join(", ")
    };
}

#[macro_export]
macro_rules! token_kinds {
    ($($kind:ident$(= $val:literal $($kw:ident)?)?),+$(,)?) => {
        #[derive(Debug, PartialEq)]
        pub enum TokenKind {
            $($kind),+
        }

        pub const COMMANDS: [&str; crate::count!($($($($kw)?)?)+)] = [
            $(
                $(
                    $(
                        crate::ignore!($kw,  casey::lower!(stringify!($val))),
                    )?
                )?
            )+
        ];

        impl<'a> Iterator for Lexer<'a> {
            type Item = Token;

            fn next(&mut self) -> Option<Token> {
                let mut text = String::new();
                while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}
                if let Some(c) = self.chars.next() {
                    use TokenKind::*;
                    text.push(c);
                    match c.to_string().as_str() {
                        $($($val => Some(Token::new($kind, text)),)?)+
                        c => {
                            let c = c.chars().nth(0).unwrap();
                            if !c.is_alphanumeric() {
                                return None;
                            }

                            if c == '#' {
                                text.pop();
                                while let Some(_) = self.chars.next_if(|x| *x != '\n') {
                                    text.push(c);
                                }
                                return Some(Token::new(Comment, text));
                            }

                            while let Some(c) = self
                                .chars
                                .next_if(|x| x.is_alphanumeric() && !x.is_whitespace())
                            {
                                text.push(c);
                            }

                            match text.as_str() {
                                $($($val => Some(Token::new($kind, text)),)?)+
                                _ => Some(Token::new(Sym, text)),
                            }
                        }
                    }
                } else {
                    None
                }
            }
        }
    };
}
