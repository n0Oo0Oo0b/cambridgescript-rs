#![feature(
    str_lines_remainder,
    stmt_expr_attributes,
    try_trait_v2,
    read_buf,
    map_try_insert,
    if_let_guard
)]

use std::io;

use interpreter::Interpreter;

mod interpreter;
mod scanner;
mod token;
mod tree_parser;

fn main() -> io::Result<()> {
    let mut i = Interpreter::new();
    i.repl()
}
