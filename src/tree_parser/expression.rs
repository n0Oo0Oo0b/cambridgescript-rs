use crate::{
    interpreter::{Assign, BoxEval, Eval},
    token::{Token, TokenType},
};
use codespan::{ByteIndex, Span};

use super::{
    enum_derive, join_span,
    parser::{token_of, Parse, ParseErrorKind, ParseResult, ParseStream},
    stmt::ProcedureDecl,
    MaybeSpanned, Value,
};

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

pub trait Pow<Rhs = Self> {
    type Output;

    fn pow(self, rhs: Rhs) -> Self::Output;
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl TokenType {
    #[inline]
    fn as_unary(&self) -> Option<(UnaryOp, Precedence)> {
        Some(match self {
            Self::Minus => (UnaryOp::Neg, Precedence::Unary),
            Self::Not => (UnaryOp::Not, Precedence::LogicNot),
            _ => return None,
        })
    }

    #[inline]
    fn as_binary(&self) -> Option<(BinaryOp, Precedence)> {
        Some(match self {
            // Logic
            Self::Or => (BinaryOp::Or, Precedence::LogicOr),
            Self::And => (BinaryOp::And, Precedence::LogicAnd),
            // Equality + comparison
            Self::Equal => (BinaryOp::Eq, Precedence::Equality),
            Self::NotEqual => (BinaryOp::Ne, Precedence::Equality),
            Self::Less => (BinaryOp::Lt, Precedence::Comparison),
            Self::Greater => (BinaryOp::Gt, Precedence::Comparison),
            Self::LessEqual => (BinaryOp::Le, Precedence::Comparison),
            Self::GreaterEqual => (BinaryOp::Ge, Precedence::Comparison),
            // Arithmetic
            Self::Plus => (BinaryOp::Add, Precedence::Term),
            Self::Minus => (BinaryOp::Sub, Precedence::Term),
            Self::Star => (BinaryOp::Mul, Precedence::Factor),
            Self::Slash => (BinaryOp::Div, Precedence::Factor),
            Self::Caret => (BinaryOp::Pow, Precedence::Exponent),
            _ => return None,
        })
    }
}

pub mod expr {
    use super::*;

    #[derive(Debug)]
    pub struct BinaryExpr {
        pub left: BoxEval,
        pub op: BinaryOp,
        pub right: BoxEval,
    }

    #[derive(Debug)]
    pub struct UnaryExpr {
        pub op: UnaryOp,
        pub(crate) op_span: Option<Span>,
        pub right: BoxEval,
    }

    #[derive(Debug)]
    pub struct FunctionCall {
        pub function: BoxEval,
        pub args: Box<[BoxEval]>,
    }

    #[derive(Debug)]
    pub struct ArrayIndex<E> {
        pub array: E,
        pub indexes: Box<[E]>,
    }

    #[derive(Debug)]
    pub struct Identifier {
        pub handle: usize,
        pub(crate) span: Option<Span>,
    }

    #[derive(Debug)]
    pub struct Literal {
        pub value: Value,
        pub(crate) span: Option<Span>,
    }
}

impl Parse for expr::Literal {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let token = stream.advance().expect("Literal token");
        Ok(Self {
            value: token.r#type.try_into().expect("Literal token"),
            span: token.span,
        })
    }
}

impl Parse for expr::Identifier {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let (ident, span) = match stream.advance() {
            Some(Token {
                r#type: TokenType::Identifier(i),
                span,
            }) => (i, span),
            _ => panic!("ident() called without identifier token"),
        };
        let handle = stream.get_ident_handle(ident);
        Ok(Self { handle, span })
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
enum Precedence {
    None,
    LogicOr,
    LogicAnd,
    LogicNot,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Exponent,
    Call,
    Primary,
}

impl Precedence {
    fn next(self) -> Self {
        if self == Self::Primary {
            panic!("Attempt to call next() on Primary")
        }
        // Safety: x+1 is always valid because self cannot be Primary
        unsafe { std::mem::transmute::<u8, Self>(self as u8 + 1) }
    }
}

fn parse_precedence(
    stream: &mut ParseStream,
    precedence: Precedence,
) -> ParseResult<Box<dyn Eval>> {
    // Prefix rule
    let mut res: Box<dyn Eval> = match stream.peek() {
        Some(
            token_of!(IntegerLiteral(_))
            | token_of!(RealLiteral(_))
            | token_of!(CharLiteral(_))
            | token_of!(StringLiteral(_))
            | token_of!(BooleanLiteral(_)),
        ) => Box::new(expr::Literal::parse(stream)?),
        Some(token_of!(Identifier(_))) => Box::new(expr::Identifier::parse(stream)?),
        Some(Token { r#type, span }) if let Some((op, prec)) = r#type.as_unary() => {
            if prec < precedence {
                todo!("unary prec handling");
            }
            stream.advance();
            Box::new(expr::UnaryExpr {
                op,
                op_span: span,
                right: parse_precedence(stream, prec.next())?,
            })
        }
        Some(token_of!(LParen)) => {
            let s = stream.advance().unwrap().span;
            let inner = parse_expr(stream)?;
            stream.force_consume(TokenType::RParen, ("'(' was not closed", s))?;
            inner
        }
        t => {
            todo!("parse_precedence/_")
        }
    };

    loop {
        // Infix rule
        res = match stream.peek() {
            Some(Token { r#type, span }) if let Some((op, prec)) = r#type.as_binary() => {
                if prec < precedence {
                    break;
                }
                stream.advance();
                Box::new(expr::BinaryExpr {
                    left: res,
                    op,
                    right: parse_precedence(stream, prec.next())?,
                })
            }
            _ => break,
        }
    }
    Ok(res)
}

pub(crate) fn parse_assignable(stream: &mut ParseStream) -> ParseResult<Box<dyn Assign>> {
    match stream.peek() {
        Some(token_of!(Identifier(_))) => Ok(Box::new(expr::Identifier::parse(stream)?)),
        _ => stream.error(ParseErrorKind::ExpectedExpression, ("", None)),
    }
}

pub fn parse_expr(stream: &mut ParseStream) -> ParseResult<Box<dyn Eval>> {
    parse_precedence(stream, Precedence::None)
}

pub(super) fn parse_arguments(stream: &mut ParseStream) -> ParseResult<Box<[BoxEval]>> {
    let mut items = vec![parse_expr(stream)?];
    while stream.consume(TokenType::Comma).is_some() {
        items.push(parse_expr(stream)?);
    }
    Ok(items.into())
}
