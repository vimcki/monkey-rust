use std::{any::Any, fmt};

use crate::lexer::lexer::Token;

pub trait Node: fmt::Debug {
    fn token(&self) -> Token;
    fn text(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub trait Statement: Node {
    fn statement_node(&self);
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
    fn as_any(&self) -> &dyn Any {
        self
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        self.expression.text()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
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
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        self.token.text()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: Box<dyn Expression>,
}

impl Node for PrefixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        format!("({}{})", self.token.text(), self.right.text())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        format!(
            "({} {} {})",
            self.left.text(),
            self.token.text(),
            self.right.text()
        )
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct BooleanExpression {
    pub token: Token,
}

impl Node for BooleanExpression {
    fn token(&self) -> Token {
        self.token.clone()
    }
    fn text(&self) -> String {
        self.token.text()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for BooleanExpression {
    fn expression_node(&self) {}
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token(&self) -> Token {
        return Token::LBRACE;
    }
    fn text(&self) -> String {
        let mut s = String::new();
        for stmt in &self.statements {
            s.push_str(&stmt.text());
        }
        return s;
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token(&self) -> Token {
        return Token::IF;
    }
    fn text(&self) -> String {
        let mut s = format!("if {} {}", self.condition.text(), self.consequence.text());
        if let Some(alt) = &self.alternative {
            s.push_str(&format!("else {}", alt.text()));
        }
        return s;
    }
    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

impl Expression for IfExpression {
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
