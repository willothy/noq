mod lexer {
    use std::{iter::Peekable, str::Chars};
    pub enum TokenKind {
        Ident,
        Comment,
        OpenParen,
        CloseParen,
        Comma,
        Equals,
        Semicolon,
        Op(OpKind),
        Command(CommandKind),
        Strategy(StrategyKind),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TokenKind::Ident => ::core::fmt::Formatter::write_str(f, "Ident"),
                TokenKind::Comment => ::core::fmt::Formatter::write_str(f, "Comment"),
                TokenKind::OpenParen => ::core::fmt::Formatter::write_str(f, "OpenParen"),
                TokenKind::CloseParen => {
                    ::core::fmt::Formatter::write_str(f, "CloseParen")
                }
                TokenKind::Comma => ::core::fmt::Formatter::write_str(f, "Comma"),
                TokenKind::Equals => ::core::fmt::Formatter::write_str(f, "Equals"),
                TokenKind::Semicolon => ::core::fmt::Formatter::write_str(f, "Semicolon"),
                TokenKind::Op(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Op", &__self_0)
                }
                TokenKind::Command(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Command",
                        &__self_0,
                    )
                }
                TokenKind::Strategy(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Strategy",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for TokenKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for TokenKind {
        #[inline]
        fn eq(&self, other: &TokenKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (TokenKind::Op(__self_0), TokenKind::Op(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (TokenKind::Command(__self_0), TokenKind::Command(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (TokenKind::Strategy(__self_0), TokenKind::Strategy(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenKind {
        #[inline]
        fn clone(&self) -> TokenKind {
            match self {
                TokenKind::Ident => TokenKind::Ident,
                TokenKind::Comment => TokenKind::Comment,
                TokenKind::OpenParen => TokenKind::OpenParen,
                TokenKind::CloseParen => TokenKind::CloseParen,
                TokenKind::Comma => TokenKind::Comma,
                TokenKind::Equals => TokenKind::Equals,
                TokenKind::Semicolon => TokenKind::Semicolon,
                TokenKind::Op(__self_0) => {
                    TokenKind::Op(::core::clone::Clone::clone(__self_0))
                }
                TokenKind::Command(__self_0) => {
                    TokenKind::Command(::core::clone::Clone::clone(__self_0))
                }
                TokenKind::Strategy(__self_0) => {
                    TokenKind::Strategy(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub enum OpKind {
        Add,
        Sub,
        Mul,
        Div,
        Pow,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for OpKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                OpKind::Add => ::core::fmt::Formatter::write_str(f, "Add"),
                OpKind::Sub => ::core::fmt::Formatter::write_str(f, "Sub"),
                OpKind::Mul => ::core::fmt::Formatter::write_str(f, "Mul"),
                OpKind::Div => ::core::fmt::Formatter::write_str(f, "Div"),
                OpKind::Pow => ::core::fmt::Formatter::write_str(f, "Pow"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for OpKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for OpKind {
        #[inline]
        fn eq(&self, other: &OpKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OpKind {
        #[inline]
        fn clone(&self) -> OpKind {
            match self {
                OpKind::Add => OpKind::Add,
                OpKind::Sub => OpKind::Sub,
                OpKind::Mul => OpKind::Mul,
                OpKind::Div => OpKind::Div,
                OpKind::Pow => OpKind::Pow,
            }
        }
    }
    #[allow(dead_code)]
    pub const MAX_PRECEDENCE: usize = 2;
    impl OpKind {
        #[allow(dead_code)]
        pub fn precedence(&self) -> usize {
            use OpKind::*;
            match self {
                Add => 0,
                Sub => 0,
                Mul => 1,
                Div => 1,
                Pow => 2,
            }
        }
    }
    impl std::fmt::Display for OpKind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use OpKind::*;
            match self {
                Add => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"+\""], &[])),
                Sub => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"-\""], &[])),
                Mul => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"*\""], &[])),
                Div => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"/\""], &[])),
                Pow => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"^\""], &[])),
            }
        }
    }
    pub enum CommandKind {
        Done,
        Shape,
        Rule,
        Apply,
        Undo,
        Redo,
        Help,
        Quit,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CommandKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CommandKind::Done => ::core::fmt::Formatter::write_str(f, "Done"),
                CommandKind::Shape => ::core::fmt::Formatter::write_str(f, "Shape"),
                CommandKind::Rule => ::core::fmt::Formatter::write_str(f, "Rule"),
                CommandKind::Apply => ::core::fmt::Formatter::write_str(f, "Apply"),
                CommandKind::Undo => ::core::fmt::Formatter::write_str(f, "Undo"),
                CommandKind::Redo => ::core::fmt::Formatter::write_str(f, "Redo"),
                CommandKind::Help => ::core::fmt::Formatter::write_str(f, "Help"),
                CommandKind::Quit => ::core::fmt::Formatter::write_str(f, "Quit"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for CommandKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for CommandKind {
        #[inline]
        fn eq(&self, other: &CommandKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CommandKind {
        #[inline]
        fn clone(&self) -> CommandKind {
            match self {
                CommandKind::Done => CommandKind::Done,
                CommandKind::Shape => CommandKind::Shape,
                CommandKind::Rule => CommandKind::Rule,
                CommandKind::Apply => CommandKind::Apply,
                CommandKind::Undo => CommandKind::Undo,
                CommandKind::Redo => CommandKind::Redo,
                CommandKind::Help => CommandKind::Help,
                CommandKind::Quit => CommandKind::Quit,
            }
        }
    }
    impl std::fmt::Display for CommandKind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use CommandKind::*;
            match self {
                Done => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"done\""], &[])),
                Shape => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"shape\""], &[])),
                Rule => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"rule\""], &[])),
                Apply => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"apply\""], &[])),
                Undo => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"undo\""], &[])),
                Redo => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"redo\""], &[])),
                Help => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"help\""], &[])),
                Quit => f.write_fmt(::core::fmt::Arguments::new_v1(&["\"quit\""], &[])),
            }
        }
    }
    pub enum StrategyKind {
        All,
        First,
        Deep,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for StrategyKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                StrategyKind::All => ::core::fmt::Formatter::write_str(f, "All"),
                StrategyKind::First => ::core::fmt::Formatter::write_str(f, "First"),
                StrategyKind::Deep => ::core::fmt::Formatter::write_str(f, "Deep"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for StrategyKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for StrategyKind {
        #[inline]
        fn eq(&self, other: &StrategyKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StrategyKind {
        #[inline]
        fn clone(&self) -> StrategyKind {
            match self {
                StrategyKind::All => StrategyKind::All,
                StrategyKind::First => StrategyKind::First,
                StrategyKind::Deep => StrategyKind::Deep,
            }
        }
    }
    #[allow(dead_code)]
    pub const COMMANDS: [&str; 1usize
        + (1usize
            + (1usize
                + (1usize + (1usize + (1usize + (1usize + (1usize + 0usize)))))))] = [
        "\"done\"",
        "\"shape\"",
        "\"rule\"",
        "\"apply\"",
        "\"undo\"",
        "\"redo\"",
        "\"help\"",
        "\"quit\"",
    ];
    #[allow(dead_code)]
    pub const BINOPS: [&str; 1usize
        + (1usize + (1usize + (1usize + (1usize + 0usize))))] = [
        "\"+\"",
        "\"-\"",
        "\"*\"",
        "\"/\"",
        "\"^\"",
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
                    "(" => Some(Token::new(OpenParen, text)),
                    ")" => Some(Token::new(CloseParen, text)),
                    "," => Some(Token::new(Comma, text)),
                    "=" => Some(Token::new(Equals, text)),
                    ";" => Some(Token::new(Semicolon, text)),
                    "+" => Some(Token::new(Op(OpKind::Add), text)),
                    "-" => Some(Token::new(Op(OpKind::Sub), text)),
                    "*" => Some(Token::new(Op(OpKind::Mul), text)),
                    "/" => Some(Token::new(Op(OpKind::Div), text)),
                    "^" => Some(Token::new(Op(OpKind::Pow), text)),
                    "done" => Some(Token::new(Command(CommandKind::Done), text)),
                    "shape" => Some(Token::new(Command(CommandKind::Shape), text)),
                    "rule" => Some(Token::new(Command(CommandKind::Rule), text)),
                    "apply" => Some(Token::new(Command(CommandKind::Apply), text)),
                    "undo" => Some(Token::new(Command(CommandKind::Undo), text)),
                    "redo" => Some(Token::new(Command(CommandKind::Redo), text)),
                    "help" => Some(Token::new(Command(CommandKind::Help), text)),
                    "quit" => Some(Token::new(Command(CommandKind::Quit), text)),
                    "all" => Some(Token::new(Strategy(StrategyKind::All), text)),
                    "first" => Some(Token::new(Strategy(StrategyKind::First), text)),
                    "deep" => Some(Token::new(Strategy(StrategyKind::Deep), text)),
                    c => {
                        let c = c.chars().nth(0).unwrap();
                        if !c.is_alphanumeric() {
                            if c == '#' {
                                while let Some(c) = self.chars.next_if(|x| *x != '\n') {
                                    text.push(c);
                                }
                                return Some(Token::new(Comment, text));
                            }
                            return None;
                        }
                        while let Some(c)
                            = self
                                .chars
                                .next_if(|x| x.is_alphanumeric() && !x.is_whitespace())
                        {
                            text.push(c);
                        }
                        match text.as_str() {
                            "(" => Some(Token::new(OpenParen, text)),
                            ")" => Some(Token::new(CloseParen, text)),
                            "," => Some(Token::new(Comma, text)),
                            "=" => Some(Token::new(Equals, text)),
                            ";" => Some(Token::new(Semicolon, text)),
                            "+" => Some(Token::new(Op(OpKind::Add), text)),
                            "-" => Some(Token::new(Op(OpKind::Sub), text)),
                            "*" => Some(Token::new(Op(OpKind::Mul), text)),
                            "/" => Some(Token::new(Op(OpKind::Div), text)),
                            "^" => Some(Token::new(Op(OpKind::Pow), text)),
                            "done" => Some(Token::new(Command(CommandKind::Done), text)),
                            "shape" => {
                                Some(Token::new(Command(CommandKind::Shape), text))
                            }
                            "rule" => Some(Token::new(Command(CommandKind::Rule), text)),
                            "apply" => {
                                Some(Token::new(Command(CommandKind::Apply), text))
                            }
                            "undo" => Some(Token::new(Command(CommandKind::Undo), text)),
                            "redo" => Some(Token::new(Command(CommandKind::Redo), text)),
                            "help" => Some(Token::new(Command(CommandKind::Help), text)),
                            "quit" => Some(Token::new(Command(CommandKind::Quit), text)),
                            "all" => Some(Token::new(Strategy(StrategyKind::All), text)),
                            "first" => {
                                Some(Token::new(Strategy(StrategyKind::First), text))
                            }
                            "deep" => {
                                Some(Token::new(Strategy(StrategyKind::Deep), text))
                            }
                            _ => Some(Token::new(Ident, text)),
                        }
                    }
                }
            } else {
                None
            }
        }
    }
    pub struct Token {
        pub kind: TokenKind,
        pub text: String,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Token {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "Token",
                "kind",
                &&self.kind,
                "text",
                &&self.text,
            )
        }
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
            Self { chars, text: String::new() }
        }
    }
    impl<'a> From<&'a str> for Lexer<'a> {
        fn from(str: &'a str) -> Self {
            Lexer::from_iter(str.chars().peekable())
        }
    }
}
