use std::{collections::HashMap, io::Cursor};

use crate::ast::Value;

pub enum RuntimeError {
    UndeclaredVariable(usize),
    UndefinedVariable(usize),
    IncompatibleTypes(Value, Value),
}

pub(super) type RuntimeResult<T> = Result<T, RuntimeError>;

pub(super) struct ProgramState {
    pub variables: HashMap<usize, Option<Value>>,
    stdout: Cursor<Vec<u8>>,
}

struct Interpreter {
    state: ProgramState,
}
