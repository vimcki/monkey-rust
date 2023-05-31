mod lexer;

use crate::lexer::lexer::Lexer;

fn main() {
    let mut l = Lexer::new(vec![0]);
    l.next_token();
}
