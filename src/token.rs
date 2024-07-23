use std::{boxed::Box, marker::PhantomData};

#[rustfmt::skip]
#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Reserved words

    Procedure, EndProcedure,
    Function, Returns, EndFunction, Return,

    If, Then, Else, EndIf,
    Case, Otherwise, EndCase,
    For, To, Step, Next,
    Repeat, Until,
    While, Do, EndWhile,

    Declare, Constant,
    Input, Output, Call,

    OpenFile, ReadFile, WriteFile, CloseFile,
    Read, Write,

    Integer, Real, Char, String, Boolean,
    Array, Of,

    And, Or, Not,

    // Symbols

    LParen, RParen, LBracket, RBracket,
    Plus, Minus, Star, Slash, Caret,
    Equal, NotEqual, LessEqual, GreaterEqual, Less, Greater,
    Comma, Colon, LArrow,

    // Others

    Identifier(Box<str>),

    CharLiteral(char),
    StringLiteral(Box<str>),
    IntegerLiteral(i64),
    RealLiteral(f64),
    BooleanLiteral(bool),

    Whitespace, Comment,
}

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub type_: TokenType,
    pub lexeme: Option<&'a str>,
}

impl<'a> Token<'a> {
    pub fn new(type_: TokenType, lexeme: &'a str) -> Self {
        Self {
            type_,
            lexeme: Some(lexeme),
        }
    }
}

impl From<TokenType> for Token<'_> {
    // Allow manual creation of Tokens without a source string
    fn from(value: TokenType) -> Self {
        Self {
            type_: value,
            lexeme: None,
        }
    }
}
