use std::fmt::Debug;

use crate::tree_parser::stmt;

use super::runtime::RuntimeResult;
use super::ProgramState;

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

impl Exec for stmt::VariableDecl {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        self.target.declare(state, self.r#type)?;
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

impl Exec for stmt::IfStmt {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        if self.condition.as_bool(state)? {
            self.then_branch.exec(state)?;
        } else {
            self.else_branch.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for stmt::WhileLoop {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        while self.condition.as_bool(state)? {
            self.body.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for stmt::UntilLoop {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        loop {
            self.body.exec(state)?;
            if self.condition.as_bool(state)? {
                break;
            }
        }
        Ok(())
    }
}

impl Exec for stmt::AssignStmt {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        self.target.assign(state, self.value.eval(state)?)?;
        Ok(())
    }
}
