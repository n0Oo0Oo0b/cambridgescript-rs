use std::{fmt::Display, rc::Rc};

use codespan::{ByteIndex, Span};

use crate::tree_parser::MaybeSpanned;

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
        match self {
            // Reserved words
            Self::Procedure => "'PROCEDURE'",
            Self::EndProcedure => "'ENDPROCEDURE'",
            Self::Function => "'FUNCTION'",
            Self::Returns => "'RETURNS'",
            Self::EndFunction => "'ENDFUNCTION'",
            Self::Return => "'RETURN'",
            Self::If => "'IF'",
            Self::Then => "'THEN'",
            Self::Else => "'ELSE'",
            Self::EndIf => "'ENDIF'",
            Self::Case => "'CASE'",
            Self::Otherwise => "'OTHERWISE'",
            Self::EndCase => "'ENDCASE'",
            Self::For => "'FOR'",
            Self::To => "'TO'",
            Self::Step => "'STEP'",
            Self::Next => "'NEXT'",
            Self::Repeat => "'REPEAT'",
            Self::Until => "'UNTIL'",
            Self::While => "'WHILE'",
            Self::Do => "'DO'",
            Self::EndWhile => "'ENDWHILE'",
            Self::Declare => "'DECLARE'",
            Self::Constant => "'CONSTANT'",
            Self::Input => "'INPUT'",
            Self::Output => "'OUTPUT'",
            Self::Call => "'CALL'",
            Self::OpenFile => "'OPENfILE'",
            Self::ReadFile => "'READfILE'",
            Self::WriteFile => "'WRITEfILE'",
            Self::CloseFile => "'CLOSEfILE'",
            Self::Read => "'READ'",
            Self::Write => "'WRITE'",
            Self::Integer => "'INTEGER'",
            Self::Real => "'REAL'",
            Self::Char => "'CHAR'",
            Self::String => "'STRING'",
            Self::Boolean => "'BOOLEAN'",
            Self::Array => "'ARRAY'",
            Self::Of => "'OF'",
            Self::And => "'AND'",
            Self::Or => "'OR'",
            Self::Not => "'NOT'",
            // Symbols
            Self::LParen => "'('",
            Self::RParen => "')'",
            Self::LBracket => "'['",
            Self::RBracket => "']'",
            Self::Plus => "'+'",
            Self::Minus => "'-'",
            Self::Star => "'*'",
            Self::Slash => "'/'",
            Self::Caret => "'^'",
            Self::Equal => "'='",
            Self::NotEqual => "'<>'",
            Self::LessEqual => "'<='",
            Self::GreaterEqual => "'>='",
            Self::Less => "'<'",
            Self::Greater => "'>'",
            Self::Comma => "','",
            Self::Colon => "':'",
            Self::LArrow => "'<-'",
            // Others
            Self::Identifier(name) => name.as_ref(),
            Self::CharLiteral(c) => return write!(f, "{c}"),
            Self::StringLiteral(s) => return write!(f, "{s}"),
            Self::IntegerLiteral(i) => return write!(f, "{i}"),
            Self::RealLiteral(r) => return write!(f, "{r}"),
            Self::BooleanLiteral(b) => return write!(f, "{b}"),
            Self::Whitespace => "[whitespace]",
            Self::Comment => "[comment]",
        }
        .fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub type_: TokenType,
    pub span: Option<Span>,
}

impl Token {
    pub fn new(type_: TokenType, span: Option<Span>) -> Self {
        Self { type_, span }
    }

    pub fn with_span(type_: TokenType, span: Span) -> Self {
        Self::new(type_, Some(span))
    }

    pub fn without_span(type_: TokenType) -> Self {
        Self::new(type_, None)
    }
}

impl From<TokenType> for Token {
    fn from(value: TokenType) -> Self {
        Self::without_span(value)
    }
}

#[derive(Debug, Clone)]
pub enum ErrorLocation {
    Token(Token),
    Eof(Option<ByteIndex>),
}

impl MaybeSpanned for ErrorLocation {
    fn get_span(&self) -> Option<Span> {
        match self {
            Self::Token(t) => t.span,
            Self::Eof(Some(b)) => Some(Span::new(*b, *b)),
            Self::Eof(None) => None,
        }
    }
}

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(t) => write!(f, "{}", t.type_),
            Self::Eof(_) => write!(f, "EOF (end of file)"),
        }
    }
}

impl From<Token> for ErrorLocation {
    fn from(value: Token) -> Self {
        Self::Token(value)
    }
}
