mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;

use std::io;

use evaluator::eval_program;
use repl::start;

use crate::lexer::lexer::Lexer;

fn main() -> Result<(), io::Error> {
    let res = start();
    let l = Lexer::new(vec![0]);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program().unwrap();
    let obj = eval_program(program);

    return res;
}
