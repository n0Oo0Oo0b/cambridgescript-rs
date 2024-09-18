#![feature(
    str_lines_remainder,
    stmt_expr_attributes,
    try_trait_v2,
    read_buf,
    map_try_insert,
    if_let_guard
)]

use std::{env, fs, io};

use interpreter::Interpreter;

mod interpreter;
mod scanner;
mod token;
mod tree_parser;

fn main() -> io::Result<()> {
    let mut i = Interpreter::new();
    let args: Vec<_> = env::args().collect();
    match args.get(1) {
        Some(fp) => {
            let script = fs::read_to_string(fp)?;
            i.full_exec(&script);
            Ok(())
        }
        None => i.repl(),
    }
}
