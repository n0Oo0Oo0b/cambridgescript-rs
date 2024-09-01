use crate::ast::{Block, Stmt, Value};

use super::{
    eval::Eval,
    runtime::{ProgramState, RuntimeResult},
};

trait Exec {
    fn exec(&self, state: &ProgramState) -> RuntimeResult<()>;
}

impl Exec for Block {
    fn exec(&self, state: &ProgramState) -> RuntimeResult<()> {
        for stmt in self.0.iter() {
            stmt.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for Stmt {
    fn exec(&self, state: &ProgramState) -> RuntimeResult<()> {
        match self {
            Stmt::Output(values) => {
                let a: Vec<_> = values
                    .iter()
                    .map(|v| v.eval(state))
                    .collect::<RuntimeResult<_>>()?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if let Value::Boolean(true) = condition.eval(state)? {
                    then_branch.exec(state)?;
                } else {
                    else_branch.exec(state)?;
                }
            }
            _ => todo!(),
        }
        Ok(())
    }
}
