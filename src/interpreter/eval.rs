use std::ops::{Add, Neg, Sub};

use super::runtime::{ProgramState, RuntimeError, RuntimeResult};
use crate::ast::{BinaryOp, Expr, UnaryOp, Value};

pub(super) trait Eval {
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
                    BinaryOp::Sub => left - right,
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}
