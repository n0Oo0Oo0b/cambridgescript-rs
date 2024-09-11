#![feature(str_lines_remainder, stmt_expr_attributes, try_trait_v2, read_buf)]

use std::io;

use interpreter::Interpreter;

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

fn main() {
    let mut i = Interpreter::new();
    if let Err(e) = i.exec_src(r#"IF TRUE THEN OUTPUT 1"#) {
        dbg!(e);
    }
    io::copy(&mut i.get_stdout(), &mut io::stdout()).expect("Couldn't copy");
}
