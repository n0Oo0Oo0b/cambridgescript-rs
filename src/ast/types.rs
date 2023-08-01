use crate::ast::Expr;

pub enum Type {
    Array(ArrayType),
    Primitive(PrimitiveType),
}

pub enum PrimitiveType {
    Char,
    String,
    Integer,
    Real,
    Boolean,
}

pub struct ArrayType {
    inner_type: PrimitiveType,
    ranges: Vec<(Expr, Expr)>,
}
