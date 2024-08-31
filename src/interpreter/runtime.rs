use std::{collections::HashMap, io::Cursor};

use crate::ast::{Expr, Value};

use super::eval::Eval;

#[derive(Debug)]
pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    IncompatibleTypes(Value, Value),
}

pub(super) type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Default)]
pub(super) struct ProgramState {
    pub variables: HashMap<usize, Option<Value>>,
    stdout: Cursor<Vec<u8>>,
}

pub struct Interpreter {
    state: ProgramState,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            state: ProgramState::default(),
        }
    }

    pub fn eval(&self, tree: Expr) -> RuntimeResult<Value> {
        tree.eval(&self.state)
    }
}
