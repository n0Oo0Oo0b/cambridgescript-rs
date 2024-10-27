use core::fmt;
use std::fmt::Debug;

use crate::{
    interpreter::{Assign, BoxEval, Eval},
    token::{Token, TokenType},
};
use codespan::{ByteIndex, Span};

use super::{
    enum_derive, join_span,
    parser::{token_of, Parse, ParseErrorKind, ParseResult, ParseStream},
    stmt::ProcedureDecl,
    MaybeSpanned, Spanned, Value,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::And => "AND",
            Self::Or => "OR",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Pow => "^",
            Self::Eq => "=",
            Self::Ne => "<>",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::Lt => "<",
            Self::Gt => ">",
        })
    }
}

pub trait Pow<Rhs = Self> {
    type Output;

    fn pow(self, rhs: Rhs) -> Self::Output;
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Not => "NOT",
            Self::Neg => "-",
        })
    }
}

#[derive(Debug, Clone, Copy)]
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
    use std::rc::Rc;

    use crate::tree_parser::Spanned;

    use super::*;

    #[derive(Debug)]
    pub struct BinaryExpr {
        pub left: BoxEval,
        pub right: BoxEval,
        pub op: Spanned<BinaryOp>,
    }

    #[derive(Debug)]
    pub struct UnaryExpr {
        pub op: Spanned<UnaryOp>,
        pub right: BoxEval,
    }

    #[derive(Debug)]
    pub struct FunctionCall {
        pub function: BoxEval,
        pub args: Box<[BoxEval]>,
    }

    #[derive(Debug)]
    pub struct ArrayIndex {
        pub array: Box<dyn Assign>,
        pub indexes: BoxEval,
    }

    #[derive(Debug)]
    pub struct Identifier {
        pub handle: usize,
        pub name: Rc<str>,
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
            value: token.inner.try_into().expect("Literal token"),
            span: token.span,
        })
    }
}

impl Parse for expr::Identifier {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let (name, span) = match stream.advance() {
            Some(Token {
                inner: TokenType::Identifier(i),
                span,
            }) => (i, span),
            _ => panic!("ident() called without identifier token"),
        };
        let handle = stream.get_ident_handle(name.clone());
        Ok(Self { handle, span, name })
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
        Some(Token { inner: item, span }) if let Some((op, prec)) = item.as_unary() => {
            if prec < precedence {
                todo!("unary prec handling");
            }
            stream.advance();
            Box::new(expr::UnaryExpr {
                op: Spanned::new(op, span),
                right: parse_precedence(stream, prec.next())?,
            })
        }
        Some(token_of!(LParen)) => {
            let s = stream.advance().unwrap().span;
            let inner = parse_expr(stream)?;
            stream.force_consume(TokenType::RParen, ("'(' was not closed", s))?;
            inner
        }
        t => return stream.error(ParseErrorKind::ExpectedExpression, ("", None)),
    };

    loop {
        // Infix rule
        res = match stream.peek() {
            Some(Token { inner: item, span }) if let Some((op, prec)) = item.as_binary() => {
                if prec < precedence {
                    break;
                }
                stream.advance();
                Box::new(expr::BinaryExpr {
                    left: res,
                    right: parse_precedence(stream, prec.next())?,
                    op: Spanned::new(op, span),
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
