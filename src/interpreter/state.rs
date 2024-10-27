use std::io::Write;
use std::mem;
use std::{collections::HashMap, fmt};

use crate::tree_parser::{PrimitiveType, Value};

#[derive(Clone)]
pub enum VariableState {
    Undeclared,
    Declared(PrimitiveType),
    Defined(Value),
}

impl Default for VariableState {
    fn default() -> Self {
        Self::Undeclared
    }
}

#[derive(Default)]
pub struct ProgramState {
    variables: HashMap<usize, VariableState>,
    pub(super) stdout: Vec<u8>,
}

impl ProgramState {
    pub fn get_var(&self, handle: usize) -> VariableState {
        self.variables
            .get(&handle)
            .cloned()
            .unwrap_or(VariableState::default())
    }

    pub fn get_var_mut(&mut self, handle: usize) -> &mut VariableState {
        self.variables.entry(handle).or_default()
    }

    pub(super) fn write(&mut self, thing: impl AsRef<str>) {
        self.stdout
            .write_all(thing.as_ref().as_bytes())
            .expect("Failed to output");
    }

    pub(super) fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) {
        self.stdout.write_fmt(fmt).expect("Failed to output");
    }

    pub(super) fn take_stdout(&mut self) -> Box<[u8]> {
        let mut value = Vec::new();
        mem::swap(&mut self.stdout, &mut value);
        value.into_boxed_slice()
    }
}
