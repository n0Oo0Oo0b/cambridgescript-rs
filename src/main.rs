#![feature(str_lines_remainder, stmt_expr_attributes, try_trait_v2, read_buf)]

use std::io;

use interpreter::Interpreter;

mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

const SOURCE: &str = r#"
IF 2 + 2 = 4 THEN
    OUTPUT "yay"
ENDIF
"#;

fn main() {
    let mut i = Interpreter::new();
    let _ = i.exec_src(SOURCE);
    io::copy(&mut i.get_stdout(), &mut io::stdout()).expect("Failed to output to stdout");
}
