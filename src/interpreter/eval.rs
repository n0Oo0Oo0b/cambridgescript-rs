use std::fmt::Debug;

use super::runtime::{RuntimeError, RuntimeResult};
use super::ProgramState;
use crate::tree_parser::{expr, BinaryOp, MaybeSpanned, Pow, PrimitiveType, UnaryOp, Value};

pub trait Eval: Debug + MaybeSpanned {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value>;
}

pub type BoxEval = Box<dyn Eval>;

pub trait Assign: Debug + MaybeSpanned {
    fn declare(&self, state: &mut ProgramState, r#type: PrimitiveType) -> RuntimeResult<()>;

    fn assign(&self, state: &mut ProgramState, value: Value) -> RuntimeResult<()>;
}

impl UnaryOp {
    fn apply(&self, right: Value) -> RuntimeResult<Value> {
        match self {
            Self::Not => !right,
            Self::Neg => -right,
        }
    }
}

macro_rules! cmp_op {
    ($lhs:ident, $rhs:ident, $method:ident) => {
        Ok($lhs
            .partial_cmp(&$rhs)
            .ok_or(RuntimeError::IncompatibleTypes($lhs, $rhs))?
            .$method()
            .into())
    };
}

impl BinaryOp {
    fn apply(&self, left: Value, right: Value) -> RuntimeResult<Value> {
        match self {
            Self::And | Self::Or => {
                let x: bool = match left.try_into() {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                let y: bool = match right.try_into() {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                Ok((if *self == Self::And { x && y } else { x || y }).into())
            }
            Self::Add => left + right,
            Self::Sub => left - right,
            Self::Mul => left * right,
            Self::Div => left / right,
            Self::Pow => left.pow(right),
            Self::Eq => cmp_op!(left, right, is_eq),
            Self::Ne => cmp_op!(left, right, is_ne),
            Self::Le => cmp_op!(left, right, is_le),
            Self::Ge => cmp_op!(left, right, is_ge),
            Self::Lt => cmp_op!(left, right, is_lt),
            Self::Gt => cmp_op!(left, right, is_gt),
        }
    }
}

impl Eval for expr::BinaryExpr {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        let left = self.left.eval(state)?;
        let right = self.right.eval(state)?;
        self.op.apply(left, right)
    }
}

impl Eval for expr::UnaryExpr {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        let right = self.right.eval(state)?;
        self.op.apply(right)
    }
}

impl Eval for expr::Identifier {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        state.get_variable(self.handle)
    }
}

impl Eval for expr::Literal {
    fn eval(&self, _state: &ProgramState) -> RuntimeResult<Value> {
        Ok(self.value.clone())
    }
}

impl Assign for expr::Identifier {
    fn declare(&self, state: &mut ProgramState, r#type: PrimitiveType) -> RuntimeResult<()> {
        state.declare_variable(self.handle, r#type)
    }

    fn assign(&self, state: &mut ProgramState, value: Value) -> RuntimeResult<()> {
        state.set_variable(self.handle, value)
    }
}
