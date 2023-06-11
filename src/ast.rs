use std::{any::Any, fmt};

use crate::lexer::lexer::Token;

pub trait Node: fmt::Debug {
    fn token(&self) -> Token;
    fn text(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

#[derive(Debug)]
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
    fn text(&self) -> String {
        let mut s = String::new();
        for stmt in &self.statements {
            s.push_str(&stmt.text());
        }
        return s;
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token(&self) -> Token {
        Token::LET
    }
    fn text(&self) -> String {
        format!("let {} = {};", self.name.text(), self.value.text())
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token(&self) -> Token {
        Token::RETURN
    }
    fn text(&self) -> String {
        format!("return {};", self.value.text())
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
}

impl Node for Identifier {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        self.token.text()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Identifier, LetStatement, Node},
        lexer::lexer::Token,
    };

    use super::Program;

    #[test]
    fn test_text() {
        let empty_program = Program { statements: vec![] };
        assert_eq!(empty_program.text(), "");

        let program = Program {
            statements: vec![Box::new(LetStatement {
                name: Identifier {
                    token: Token::IDENT("myVar".to_string()),
                },
                value: Box::new(Identifier {
                    token: Token::IDENT("anotherVar".to_string()),
                }),
            })],
        };
        assert_eq!(program.text(), "let myVar = anotherVar;");
    }
}
