use crate::interpreter::BoxEval;
use codespan::{ByteIndex, Span};

use super::{join_span, MaybeSpanned, Value};

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
    pub struct ArrayIndex {
        pub array: BoxEval,
        pub indexes: Box<[BoxEval]>,
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
