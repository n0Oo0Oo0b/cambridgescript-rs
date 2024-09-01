use std::rc::Rc;

#[derive(Debug)]
pub enum BinaryOp {
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug)]
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
