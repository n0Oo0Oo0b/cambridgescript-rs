use std::cmp::Ordering;

use super::runtime::{ProgramState, RuntimeError, RuntimeResult};
use crate::ast::{BinaryOp, Expr, Pow, UnaryOp, Value};

pub trait Eval {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value>;
}

impl Value {
    fn maybe_order(self, other: Self) -> RuntimeResult<Ordering> {
        self.partial_cmp(&other)
            .ok_or(RuntimeError::IncompatibleTypes(self, other))
    }

    fn apply_binary(op: &BinaryOp, left: Self, right: Self) -> RuntimeResult<Self> {
        match op {
            BinaryOp::And | BinaryOp::Or => {
                let x: bool = match left.try_into() {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                let y: bool = match right.try_into() {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                Ok((if *op == BinaryOp::And { x && y } else { x || y }).into())
            }
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            BinaryOp::Pow => left.pow(right),
            BinaryOp::Eq => Ok(left.maybe_order(right)?.is_eq().into()),
            BinaryOp::Ne => Ok(left.maybe_order(right)?.is_ne().into()),
            BinaryOp::Le => Ok(left.maybe_order(right)?.is_le().into()),
            BinaryOp::Ge => Ok(left.maybe_order(right)?.is_ge().into()),
            BinaryOp::Lt => Ok(left.maybe_order(right)?.is_lt().into()),
            BinaryOp::Gt => Ok(left.maybe_order(right)?.is_gt().into()),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        match self {
            Self::Identifier { handle } => state
                .variables
                .get(handle)
                .ok_or(RuntimeError::UndeclaredVariable(*handle))?
                .clone()
                .ok_or(RuntimeError::UndefinedVariable(*handle)),
            Self::Literal(val) => Ok(val.clone()),
            Self::Unary { op, right } => {
                let right = right.eval(state)?;
                match op {
                    UnaryOp::Neg => -right,
                    UnaryOp::Not => !right,
                }
            }
            Self::Binary { op, left, right } => {
                let left = left.eval(state)?;
                let right = right.eval(state)?;
                Value::apply_binary(op, left, right)
            }
            _ => todo!("Expr::eval"),
        }
    }
}
