use crate::{
    interpreter::{BoxEval, Eval},
    token::{Token, TokenType},
};
use codespan::{ByteIndex, Span};

use super::{
    enum_derive, join_span,
    parser::{token_of, Parse, ParseResult, ParseStream},
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
            value: token.type_.try_into().expect("Literal token"),
            span: token.span,
        })
    }
}

impl Parse for expr::Identifier {
    fn parse(stream: &mut ParseStream) -> ParseResult<Self> {
        let (ident, span) = match stream.advance() {
            Some(Token {
                type_: TokenType::Identifier(i),
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
            panic!("Precedence::Primary is the highest precedence")
        }
        // Safety: x+1 is always valid because self cannot be Primary
        unsafe { std::mem::transmute::<u8, Self>(self as u8 + 1) }
    }
}

fn parse_precedence(stream: &mut ParseStream) {
    // Unary rule
    match stream.peek() {
        Some(token_of!(Minus)) => todo!(),
        _ => todo!(),
    }
}

pub fn parse_expr(stream: &mut ParseStream) -> ParseResult<Box<dyn Eval>> {
    todo!("parse_expr")
}

pub(super) fn parse_arguments(stream: &mut ParseStream) -> ParseResult<Box<[BoxEval]>> {
    let mut items = vec![parse_expr(stream)?];
    while stream.consume(TokenType::Comma).is_some() {
        items.push(parse_expr(stream)?);
    }
    Ok(items.into())
}
