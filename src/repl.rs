use crate::lexer::lexer::Lexer;
use crate::lexer::lexer::Token;

use std::io::{self, Write};

pub fn start() -> Result<(), io::Error> {
    loop {
        let mut input = String::new();
        print!(">> "); // Print the prompt
        io::stdout().flush()?; // Make sure the prompt is displayed immediately

        io::stdin().read_line(&mut input)?;
        println!();
        let mut l = Lexer::new(input.as_bytes().to_vec());
        let mut token = l.next_token();
        while token != Token::EOF {
            println!("{:?}", token);
            token = l.next_token();
        }
    }
}
