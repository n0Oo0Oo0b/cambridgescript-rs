use std::io::Write;

use crate::ast::{Block, Expr, Stmt};

use super::{
    eval::Eval,
    runtime::{ProgramState, RuntimeResult},
};

pub trait Exec {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()>;
}

impl Exec for Block {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        for stmt in self.0.iter() {
            stmt.exec(state)?;
        }
        Ok(())
    }
}

impl Exec for Stmt {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        match self {
            Stmt::Output(exprs) => {
                // Collect potential errors first
                let values: Vec<_> = exprs
                    .iter()
                    .map(|v| v.eval(state))
                    .collect::<RuntimeResult<_>>()?;
                for value in values {
                    state
                        .stdout
                        .write_fmt(format_args!("{value}"))
                        .expect("Failed to write");
                }
                state
                    .stdout
                    .write_all("\n".as_bytes())
                    .expect("Failed to write");
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = condition.eval(state)?;
                if cond.try_into()? {
                    then_branch.exec(state)?;
                } else {
                    else_branch.exec(state)?;
                }
            }
            // TODO: Figure out array indexing
            Stmt::Assignment {
                target: Expr::Identifier { handle },
                value,
            } => {
                // TODO: enforce declarations
                state.variables.insert(*handle, Some(value.eval(state)?));
            }
            _ => todo!("Stmt::exec"),
        }
        Ok(())
    }
}
