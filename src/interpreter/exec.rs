use std::fmt::Debug;

use crate::ast::stmt;

use super::runtime::{ProgramState, RuntimeResult};

pub trait Exec: Debug {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()>;
}

pub type BoxExec = Box<dyn Exec>;

impl Exec for [BoxExec] {
    fn exec(&self, state: &mut ProgramState) -> RuntimeResult<()> {
        for stmt in self {
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

// match self {
// Stmt::If {
//     condition,
//     then_branch,
//     else_branch,
// } => {
//     let cond = condition.eval(state)?;
//     if cond.try_into()? {
//         then_branch.exec(state)?;
//     } else {
//         else_branch.exec(state)?;
//     }
// }
// // TODO: Figure out array indexing
// Stmt::Assignment {
//     target: Expr::Identifier { handle },
//     value,
// } => {
//     // TODO: enforce declarations
//     state.variables.insert(*handle, Some(value.eval(state)?));
// }
// _ => todo!("Stmt::exec"),
// }
