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
    pub inner_type: PrimitiveType,
    pub ranges: Vec<(Expr, Expr)>,
}
