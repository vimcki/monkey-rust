mod lexer;
mod repl;

use std::io;

use repl::start;

fn main() -> Result<(), io::Error> {
    let res = start();
    res
}
