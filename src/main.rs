#![feature(
    str_lines_remainder,
    stmt_expr_attributes,
    try_trait_v2,
    read_buf,
    map_try_insert
)]

use std::io;

use interpreter::Interpreter;

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

const SOURCE: &str = r#"
a <-
OUTPUT a + 1
a <- a + 1
OUTPUT a + 10
"#;

fn main() {
    let mut i = Interpreter::new();
    let _ = i.exec_src(SOURCE);
    io::copy(&mut i.get_stdout(), &mut io::stdout()).expect("Failed to output to stdout");
}
