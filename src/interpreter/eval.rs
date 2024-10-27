use std::fmt::Debug;
use std::rc::Rc;

use super::runtime::{RuntimeError, RuntimeResult};
use super::state::VariableState;
use super::ProgramState;
use crate::tree_parser::{expr, BinaryOp, MaybeSpanned, Pow, PrimitiveType, UnaryOp, Value};

pub trait Eval: Debug + MaybeSpanned {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value>;
}

impl dyn Eval {
    pub fn as_bool(&self, state: &ProgramState) -> RuntimeResult<bool> {
        match self.eval(state)? {
            Value::Boolean(b) => Ok(b),
            val => Err(RuntimeError::InvalidBool {
                tree: self,
                value: val,
            }),
        }
    }
}

pub type BoxEval = Box<dyn Eval>;

pub trait Assign: Debug + MaybeSpanned {
    fn declare(&self, state: &mut ProgramState, r#type: PrimitiveType) -> RuntimeResult<()>;

    fn assign(&self, state: &mut ProgramState, value: Value) -> RuntimeResult<()>;

    fn get_name(&self) -> Rc<str>;
}

macro_rules! cmp_op {
    ($lhs:expr, $rhs:expr, $method:ident) => {
        $lhs.partial_cmp(&$rhs).map(|x| x.$method().into())
    };
}

impl Eval for expr::BinaryExpr {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        let left = self.left.eval(state)?;
        let right = self.right.eval(state)?;
        let (lhs, rhs) = (left.clone(), right.clone());

        match self.op {
            // Short-circuit logic
            BinaryOp::And => Some(
                if !self.left.as_bool(state)? {
                    false
                } else {
                    self.right.as_bool(state)?
                }
                .into(),
            ),
            BinaryOp::Or => Some(
                if self.left.as_bool(state)? {
                    true
                } else {
                    self.right.as_bool(state)?
                }
                .into(),
            ),

            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            BinaryOp::Pow => left.pow(right),
            BinaryOp::Eq => cmp_op!(left, right, is_eq),
            BinaryOp::Ne => cmp_op!(left, right, is_ne),
            BinaryOp::Le => cmp_op!(left, right, is_le),
            BinaryOp::Ge => cmp_op!(left, right, is_ge),
            BinaryOp::Lt => cmp_op!(left, right, is_lt),
            BinaryOp::Gt => cmp_op!(left, right, is_gt),
        }
        .ok_or(RuntimeError::InvalidBinOpTypes {
            tree: self,
            lhs,
            rhs,
        })
    }
}

impl Eval for expr::UnaryExpr {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        let right = self.right.eval(state)?;
        let rhs = right.clone();

        match self.op {
            UnaryOp::Not => !right,
            UnaryOp::Neg => -right,
        }
        .ok_or(RuntimeError::InvalidUnOpType { tree: self, rhs })
    }
}

impl Eval for expr::Identifier {
    fn eval(&self, state: &ProgramState) -> RuntimeResult<Value> {
        match state.get_var(self.handle) {
            VariableState::Undeclared => Err(RuntimeError::UndeclaredVariable {
                var: self,
                required_type: None,
            }),
            VariableState::Declared(_) => Err(RuntimeError::UndefinedVariable { var: self }),
            VariableState::Defined(v) => Ok(v),
        }
    }
}

impl Eval for expr::Literal {
    fn eval(&self, _state: &ProgramState) -> RuntimeResult<Value> {
        Ok(self.value.clone())
    }
}

impl Assign for expr::Identifier {
    fn declare(&self, state: &mut ProgramState, r#type: PrimitiveType) -> RuntimeResult<()> {
        match state.get_var_mut(self.handle) {
            vs @ VariableState::Undeclared => *vs = VariableState::Declared(r#type),
            _ => todo!("Handle re-declaration of variables"),
        };
        Ok(())
    }

    fn assign(&self, state: &mut ProgramState, value: Value) -> RuntimeResult<()> {
        let vs = state.get_var_mut(self.handle);
        let r#type = match vs {
            VariableState::Undeclared => {
                return Err(RuntimeError::UndeclaredVariable {
                    var: self,
                    required_type: Some(value.get_type()),
                })
            }
            VariableState::Declared(t) => *t,
            VariableState::Defined(v) => v.get_type(),
        };
        if r#type == value.get_type() {
            *vs = VariableState::Defined(value);
            Ok(())
        } else {
            Err(RuntimeError::InvalidAssignType {
                assignment: todo!("Assign"),
                expected_type: r#type,
                value,
            })
        }
    }

    fn get_name(&self) -> Rc<str> {
        self.name.clone()
    }
}
