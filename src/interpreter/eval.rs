use std::ops::{Add, Neg};

use super::interpreter::{ProgramState, RuntimeError, RuntimeResult};
use crate::ast::{BinaryOp, Expr, UnaryOp, Value};

impl Add for Value {
    type Output = RuntimeResult<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            _ => todo!(),
        }
    }
}

impl Neg for Value {
    type Output = RuntimeResult<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Integer(x) => Ok(Value::Integer(-x)),
            _ => todo!(),
        }
    }
}

trait Eval {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value>;
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
                    _ => todo!(),
                }
            }
            Self::Binary { op, left, right } => {
                let left = left.eval(state)?;
                let right = right.eval(state)?;
                match op {
                    BinaryOp::Add => left + right,
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
