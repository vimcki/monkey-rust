use std::any::Any;

use crate::lexer::lexer::Token;

pub trait Node {
    fn token(&self) -> Token;
}

pub trait Statement: Node {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token(&self) -> Token {
        if self.statements.len() > 0 {
            self.statements[0].token()
        } else {
            Token::EOF
        }
    }
}

pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token(&self) -> Token {
        Token::LET
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Identifier {
    pub token: Token,
}

impl Node for Identifier {
    fn token(&self) -> Token {
        self.token.clone()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}
