use std::{iter::Peekable, str::Chars};

crate::token_kinds! {
    Sym,
    OpenParen = "(",
    CloseParen = ")",
    Comma = ",",
    Equals = "=",
    Semicolon = ";",
    Done = "done",
    Shape = "shape",
    Rule = "rule",
    Apply = "apply",
    All = "all",
    Undo = "undo",
    Redo = "redo",
    Help = "help",
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

    pub fn is_done(&mut self) -> bool {
        if let Some(c) = self.chars.peek() {
            if c.is_whitespace() {
                if self.chars.next().is_none() {
                    return true;
                }
                return self.is_done();
            }
            return false;
        } else {
            return true;
        }
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(str: &'a str) -> Self {
        Lexer::from_iter(str.chars().peekable())
    }
}

#[macro_export]
macro_rules! token_kinds {
    ($($kind:ident$(= $val:literal)?),+$(,)?) => {
        #[derive(Debug, PartialEq)]
        pub enum TokenKind {
            $($kind),+
        }

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
