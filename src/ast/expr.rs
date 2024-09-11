use std::{fmt, rc::Rc};

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

#[derive(Debug)]
#[allow(unused)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    FunctionCall {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
    ArrayIndex {
        array: Box<Expr>,
        indexes: Vec<Expr>,
    },
    Identifier {
        handle: usize,
    },
    Literal(Value),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Char(char),
    String(Rc<str>),
    Integer(i64),
    Real(f64),
    Boolean(bool),
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
