use crate::ast::Expr;

#[derive(Debug)]
pub enum Type {
    Array(ArrayType),
    Primitive(PrimitiveType),
}

#[derive(Debug)]
pub enum PrimitiveType {
    Char,
    String,
    Integer,
    Real,
    Boolean,
}

#[derive(Debug)]
pub struct ArrayType {
    inner_type: PrimitiveType,
    ranges: Vec<(Expr, Expr)>,
}
