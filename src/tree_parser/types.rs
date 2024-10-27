use std::{fmt, rc::Rc};

use crate::interpreter::Eval;
use crate::token::{Token, TokenType};

use super::parser::{token_of, Parse, ParseErrorKind};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrimitiveType {
    Char,
    String,
    Integer,
    Real,
    Boolean,
}

impl Parse for PrimitiveType {
    fn parse(stream: &mut super::parser::ParseStream) -> super::parser::ParseResult<Self> {
        let result = match stream.peek() {
            Some(token_of!(Char)) => Self::Char,
            Some(token_of!(String)) => Self::String,
            Some(token_of!(Integer)) => Self::Integer,
            Some(token_of!(Real)) => Self::Real,
            Some(token_of!(Boolean)) => Self::Boolean,
            tok => {
                return stream.error(ParseErrorKind::ExpectedType, ("", tok.and_then(|t| t.span)))
            }
        };
        stream.advance();
        Ok(result)
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Char => "CHAR",
            Self::String => "STRING",
            Self::Integer => "INTEGER",
            Self::Real => "REAL",
            Self::Boolean => "BOOLEAN",
        }
        .fmt(f)
    }
}

#[derive(Debug)]
pub struct ArrayType {
    pub inner_type: PrimitiveType,
    pub ranges: [(Box<dyn Eval>, Box<dyn Eval>)],
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Char(char),
    String(Rc<str>),
    Integer(i64),
    Real(f64),
    Boolean(bool),
}

impl Value {
    pub fn get_type(&self) -> PrimitiveType {
        match self {
            Self::Char(_) => PrimitiveType::Char,
            Self::String(_) => PrimitiveType::String,
            Self::Integer(_) => PrimitiveType::Integer,
            Self::Real(_) => PrimitiveType::Real,
            Self::Boolean(_) => PrimitiveType::Boolean,
        }
    }
}

impl TryFrom<TokenType> for Value {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::CharLiteral(c) => Ok(Self::Char(c)),
            TokenType::StringLiteral(s) => Ok(Self::String(s)),
            TokenType::IntegerLiteral(i) => Ok(Self::Integer(i)),
            TokenType::RealLiteral(r) => Ok(Self::Real(r)),
            TokenType::BooleanLiteral(b) => Ok(Self::Boolean(b)),
            _ => Err(()),
        }
    }
}

macro_rules! impl_from {
    ($variant:ident($t:ty)) => {
        impl From<$t> for Value {
            fn from(value: $t) -> Self {
                Value::$variant(value)
            }
        }
    };
}

impl_from!(Char(char));
impl_from!(String(Rc<str>));
impl_from!(Integer(i64));
impl_from!(Real(f64));
impl_from!(Boolean(bool));

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char(c) => c.fmt(f),
            Self::String(s) => s.as_ref().fmt(f),
            Self::Integer(i) => i.fmt(f),
            Self::Real(r) => r.fmt(f),
            Self::Boolean(b) => b.fmt(f),
        }
    }
}
