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

const SOURCE: &str = r#"
a <- 1

WHILE a < 10 DO
    a <- a + 1
ENDWHILE

REPEAT
    a <- a - 1
UNTIL a < 5

IF a = 10 THEN
    OUTPUT "a is 10"
ELSE
    OUTPUT a
ENDIF
"#;

fn main() {
    let mut i = Interpreter::new();
    let _ = i.exec_src(SOURCE);
    io::copy(&mut i.get_stdout(), &mut io::stdout()).expect("Failed to output to stdout");
}
