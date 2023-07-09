use crate::lexer::lexer::Lexer;

use crate::parser::Parser;

use std::io::{self, Write};

pub fn start() -> Result<(), io::Error> {
    loop {
        let mut input = String::new();
        print!(">> "); // Print the prompt
        io::stdout().flush()?; // Make sure the prompt is displayed immediately

        io::stdin().read_line(&mut input)?;
        println!();
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        match program {
            Ok(p) => println!("{}", p.text()),
            Err(e) => println!("{}", e),
        }
    }
}
