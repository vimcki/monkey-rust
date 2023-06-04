mod ast;
mod lexer;
mod parser;
mod repl;

use std::io;

use repl::start;

use crate::lexer::lexer::Lexer;

fn main() -> Result<(), io::Error> {
    let res = start();
    let l = Lexer::new(vec![0]);
    let mut p = parser::Parser::new(l);
    p.parse_program();
    return res;
}
