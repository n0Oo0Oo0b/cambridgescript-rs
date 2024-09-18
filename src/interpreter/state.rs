use std::io::Write;
use std::{collections::HashMap, fmt};

use crate::tree_parser::{PrimitiveType, Value};

use super::runtime::{RuntimeError, RuntimeResult};

#[derive(Default)]
pub struct ProgramState {
    variables: HashMap<usize, (PrimitiveType, Option<Value>)>,
    pub(super) stdout: Vec<u8>,
}

impl ProgramState {
    pub fn get_variable(&self, handle: usize) -> RuntimeResult<Value> {
        Ok(self
            .variables
            .get(&handle)
            .ok_or(RuntimeError::UndeclaredVariable(handle))?
            .1
            .as_ref()
            .ok_or(RuntimeError::UndefinedVariable(handle))?
            .clone())
    }

    pub fn declare_variable(&mut self, handle: usize, r#type: PrimitiveType) -> RuntimeResult<()> {
        self.variables.insert(handle, (r#type, None));
        Ok(())
    }

    pub fn set_variable(&mut self, handle: usize, value: Value) -> RuntimeResult<()> {
        let (r#type, current) = self
            .variables
            .get_mut(&handle)
            .ok_or(RuntimeError::UndeclaredVariable(handle))?;
        if *r#type != value.get_type() {
            return Err(RuntimeError::InvalidAssignmentType(handle, value));
        }
        let _ = current.insert(value);
        Ok(())
    }

    pub(super) fn write(&mut self, thing: impl AsRef<str>) {
        self.stdout
            .write_all(thing.as_ref().as_bytes())
            .expect("Failed to output");
    }

    pub(super) fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) {
        self.stdout.write_fmt(fmt).expect("Failed to output");
    }
}
