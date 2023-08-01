#[derive(Debug)]
pub enum BinaryOperator {
    LogicAnd,
    LogicOr,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
}

#[derive(Debug)]
pub enum UnaryOperator {
    LogicNot,
}

pub enum Expr {
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    FunctionCall(Box<FunctionCallExpr>),
    ArrayIndex(Box<ArrayIndexExpr>),
    Literal(Literal),
}

pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Expr,
}

pub struct FunctionCallExpr {
    pub function: Expr,
    pub args: Vec<Expr>,
}

pub struct ArrayIndexExpr {
    pub array: Expr,
    pub indices: Vec<Expr>,
}

#[derive(Debug)]
pub enum Literal {
    Char(char),
    String(String),
    Integer(i64),
    Real(f64),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Identifier {
    handle: u64,
}
