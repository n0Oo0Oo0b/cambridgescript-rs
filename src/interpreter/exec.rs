use std::fmt::Debug;

use crate::tree_parser::stmt;

use super::runtime::{ProgramState, RuntimeResult};

pub trait Exec: Debug {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()>;
}

pub type BoxExec = Box<dyn Exec>;

impl Exec for stmt::Block {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        for stmt in self.0.iter() {
            stmt.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for stmt::Output {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        let values: Box<[_]> = self
            .0
            .iter()
            .map(|expr| expr.eval(state))
            .collect::<RuntimeResult<_>>()?;
        for value in values {
            state.write_fmt(format_args!("{value}"));
        }
        state.write("\n");
        Ok(())
    }
}

impl Exec for stmt::Input {
    fn exec(&self, _state: &mut ProgramState) -> RuntimeResult<()> {
        todo!("Exec input")
    }
}

impl Exec for stmt::If {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        let cond = self.condition.eval(state)?;
        if cond.try_into()? {
            self.then_branch.exec(state)?;
        } else {
            self.else_branch.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for stmt::ConditionalLoop {
    fn exec(&self, _state: &mut ProgramState) -> RuntimeResult<()> {
        todo!("Exec loop")
    }
}

impl Exec for stmt::Assignment {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        state.set_variable(self.target.handle, self.value.eval(state)?)?;
        Ok(())
    }
}
