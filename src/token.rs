use std::{fmt::Display, marker::PhantomData, rc::Rc};

use codespan::{FileId, Span};

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
    Identifier(Rc<str>),
    CharLiteral(char),
    StringLiteral(Rc<str>),
    IntegerLiteral(i64),
    RealLiteral(f64),
    BooleanLiteral(bool),
    Whitespace, Comment,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[rustfmt::skip]
        match self {
            // Reserved words
            Self::Procedure => "'PROCEDURE'", Self::EndProcedure => "'ENDPROCEDURE'",
            Self::Function => "'FUNCTION'", Self::Returns => "'RETURNS'",
            Self::EndFunction => "'ENDFUNCTION'", Self::Return => "'RETURN'", Self::If => "'IF'",
            Self::Then => "'THEN'", Self::Else => "'ELSE'", Self::EndIf => "'ENDIF'",
            Self::Case => "'CASE'", Self::Otherwise => "'OTHERWISE'", Self::EndCase => "'ENDCASE'",
            Self::For => "'FOR'", Self::To => "'TO'", Self::Step => "'STEP'",
            Self::Next => "'NEXT'", Self::Repeat => "'REPEAT'", Self::Until => "'UNTIL'",
            Self::While => "'WHILE'", Self::Do => "'DO'", Self::EndWhile => "'ENDWHILE'",
            Self::Declare => "'DECLARE'", Self::Constant => "'CONSTANT'", Self::Input => "'INPUT'",
            Self::Output => "'OUTPUT'", Self::Call => "'CALL'", Self::OpenFile => "'OPENfILE'",
            Self::ReadFile => "'READfILE'", Self::WriteFile => "'WRITEfILE'",
            Self::CloseFile => "'CLOSEfILE'", Self::Read => "'READ'", Self::Write => "'WRITE'",
            Self::Integer => "'INTEGER'", Self::Real => "'REAL'", Self::Char => "'CHAR'",
            Self::String => "'STRING'", Self::Boolean => "'BOOLEAN'", Self::Array => "'ARRAY'",
            Self::Of => "'OF'", Self::And => "'AND'", Self::Or => "'OR'", Self::Not => "'NOT'",
            // Symbols
            Self::LParen => "'('", Self::RParen => "')'", Self::LBracket => "'['",
            Self::RBracket => "']'", Self::Plus => "'+'", Self::Minus => "'-'", Self::Star => "'*'",
            Self::Slash => "'/'", Self::Caret => "'^'", Self::Equal => "'='",
            Self::NotEqual => "'<>'", Self::LessEqual => "'<='", Self::GreaterEqual => "'>='",
            Self::Less => "'<'", Self::Greater => "'>'", Self::Comma => "','", Self::Colon => "':'",
            Self::LArrow => "'<-'",
            // Others
            Self::Identifier(name) => name.as_ref(), Self::CharLiteral(c) => "",
            Self::StringLiteral(s) => "", Self::IntegerLiteral(i) => "", Self::RealLiteral(f) => "",
            Self::BooleanLiteral(b) => "", Self::Whitespace => "whitespace",
            Self::Comment => "comment",
        }
        .fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub type_: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(type_: TokenType, span: Span) -> Self {
        Self { type_, span }
    }
}
