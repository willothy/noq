use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Sym,
    OpenParen,
    CloseParen,
    Comma,
    Equals,
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

pub struct Lexer<Chars: Iterator<Item = char>> {
    chars: Peekable<Chars>,
}

impl<Chars> Lexer<Chars>
where
    Chars: Iterator<Item = char>,
{
    pub fn from_iter(chars: Chars) -> Self {
        Self {
            chars: chars.peekable(),
        }
    }
}

impl<'a> From<&'a str> for Lexer<std::str::Chars<'a>> {
    fn from(str: &'a str) -> Self {
        Lexer::from_iter(str.chars())
    }
}

impl<Chars> Iterator for Lexer<Chars>
where
    Chars: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut text = String::new();
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}
        if let Some(c) = self.chars.next() {
            use TokenKind::*;
            text.push(c);
            match c {
                '(' => Some(Token::new(OpenParen, text)),
                ')' => Some(Token::new(CloseParen, text)),
                ',' => Some(Token::new(Comma, text)),
                '=' => Some(Token::new(Equals, text)),
                _ => {
                    if !c.is_alphanumeric() {
                        panic!("Unexpected token: {}", c);
                    }

                    while let Some(c) = self.chars.next_if(|x| x.is_alphanumeric()) {
                        text.push(c);
                    }

                    Some(Token::new(Sym, text))
                }
            }
        } else {
            None
        }
    }
}
