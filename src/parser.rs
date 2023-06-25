use std::collections::HashMap;

use crate::{
    ast::{
        BooleanExpression, Expression, ExpressionStatement, Identifier, InfixExpression,
        IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
    },
    lexer::lexer::{Lexer, Token},
};

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

type FrefixParserFn = fn(&mut Parser) -> Result<Box<dyn Expression>, String>;
type InfixParserFn = fn(&mut Parser, Box<dyn Expression>) -> Result<Box<dyn Expression>, String>;

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
    precedences: HashMap<Token, Precedence>,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            precedences: HashMap::new(),
        };

        p.precedences.insert(Token::EQUAL, Precedence::EQUALS);
        p.precedences.insert(Token::NOTEQUAL, Precedence::EQUALS);
        p.precedences.insert(Token::LT, Precedence::LESSGREATER);
        p.precedences.insert(Token::GT, Precedence::LESSGREATER);
        p.precedences.insert(Token::PLUS, Precedence::SUM);
        p.precedences.insert(Token::MINUS, Precedence::SUM);
        p.precedences.insert(Token::SLASH, Precedence::PRODUCT);
        p.precedences.insert(Token::ASTERISK, Precedence::PRODUCT);

        p.next_token();
        p.next_token();

        return p;
    }

    fn get_prefix_fn(&self, t: Token) -> Result<FrefixParserFn, String> {
        return match t {
            Token::IDENT(_) => Ok(Parser::parse_identifier),
            Token::INT(_) => Ok(Parser::parse_integer_literal),
            Token::BANG | Token::MINUS => Ok(Parser::parse_prefix_expression),
            Token::TRUE | Token::FALSE => Ok(Parser::parse_boolean),
            _ => Err("get_prefix_fn: no prefix parse function for token".to_string()),
        };
    }

    fn get_infix_fn(&self, t: Token) -> Result<InfixParserFn, String> {
        return match t {
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK
            | Token::EQUAL
            | Token::NOTEQUAL
            | Token::LT
            | Token::GT => Ok(Parser::parse_infix_expression),
            _ => Err("get_infix_fn: no infix parse function for token".to_string()),
        };
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }
        return Ok(program);
    }

    fn parse_statement(&mut self) -> Result<Box<dyn Statement>, String> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, String> {
        if !self.expect_peek(Token::IDENT("".to_string())) {
            return Err("parse_let_statement: expect_peek failed".to_string());
        }
        let name = Identifier {
            token: self.cur_token.clone(),
        };
        if !self.expect_peek(Token::ASSIGN) {
            return Err("parse_let_statement: expect_peek failed".to_string());
        }
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        return Ok(Box::new(LetStatement {
            name,
            value: Box::new(Identifier {
                token: Token::IDENT("".to_string()),
            }),
        }));
    }

    fn parse_return_statement(&mut self) -> Result<Box<dyn Statement>, String> {
        self.next_token();
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        return Ok(Box::new(ReturnStatement {
            value: Box::new(Identifier {
                token: Token::IDENT("".to_string()),
            }),
        }));
    }

    fn parse_expression_statement(&mut self) -> Result<Box<dyn Statement>, String> {
        let expr = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: expr,
        };
        return Ok(Box::new(stmt));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<dyn Expression>, String> {
        let func = self.get_prefix_fn(self.cur_token.clone())?;
        let mut left_exp = func(self)?;

        while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek_precedence() {
            let func = self.get_infix_fn(self.peek_token.clone())?;
            self.next_token();
            left_exp = func(self, left_exp)?;
        }
        return Ok(left_exp);
    }
    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        let my = format!("{:?}", self.peek_token);
        let my = my.split("(").next().unwrap().to_string();

        let other = format!("{:?}", t);
        let other = other.split("(").next().unwrap().to_string();

        my == other
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }

    fn peek_precedence(&self) -> Precedence {
        return match self.precedences.get(&self.peek_token) {
            Some(p) => p.clone(),
            None => Precedence::LOWEST,
        };
    }

    fn cur_precedence(&self) -> Precedence {
        return match self.precedences.get(&self.cur_token) {
            Some(p) => p.clone(),
            None => Precedence::LOWEST,
        };
    }

    fn parse_identifier(&mut self) -> Result<Box<dyn Expression>, String> {
        Ok(Box::new(Identifier {
            token: self.cur_token.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Box<dyn Expression>, String> {
        let value = match self.cur_token.clone() {
            Token::INT(s) => s.parse::<i64>().unwrap(),
            _ => return Err("parse_integer_literal: not an integer".to_string()),
        };
        return Ok(Box::new(IntegerLiteral {
            token: self.cur_token.clone(),
            value,
        }));
    }

    fn parse_boolean(&mut self) -> Result<Box<dyn Expression>, String> {
        return Ok(Box::new(BooleanExpression {
            token: self.cur_token.clone(),
        }));
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<dyn Expression>, String> {
        let operator = self.cur_token.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX)?;
        return Ok(Box::new(PrefixExpression {
            token: operator,
            right,
        }));
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, String> {
        let token = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        return Ok(Box::new(InfixExpression { token, left, right }));
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::ast::BooleanExpression;
    use crate::ast::Expression;
    use crate::ast::ExpressionStatement;
    use crate::ast::Identifier;
    use crate::ast::InfixExpression;
    use crate::ast::IntegerLiteral;
    use crate::ast::Node;
    use crate::ast::PrefixExpression;
    use crate::parser::Parser;
    use crate::parser::Token;
    use crate::{
        ast::{LetStatement, Statement},
        lexer::lexer::Lexer,
    };

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.is_ok(), true);
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        let tests = vec!["x", "y", "foobar"];
        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, tt);
        }
    }

    fn test_let_statement(s: &Box<dyn Statement>, name: &str) {
        assert_eq!(s.token(), Token::LET);
        let let_stmt = s.as_any().downcast_ref::<LetStatement>().unwrap();
        assert_eq!(let_stmt.name.token, Token::IDENT(name.to_string()));
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.is_ok(), true);
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 3);
        for stmt in program.statements {
            assert_eq!(stmt.token(), Token::RETURN);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert!(
            program.is_ok(),
            "Expected parsing to succeed. Error: {:?}",
            program.err()
        );
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
        test_identifier(&exp.expression, "foobar");
    }

    fn test_identifier(exp: &Box<dyn Expression>, value: &str) {
        let ident = exp.as_ref().as_any().downcast_ref::<Identifier>().unwrap();

        let literal = match ident.token {
            Token::IDENT(ref s) => s,
            _ => panic!("not ident"),
        };

        assert_eq!(literal, value);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert!(
            program.is_ok(),
            "Expected parsing to succeed. Error: {:?}",
            program.err()
        );
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
        let literal = exp
            .expression
            .as_ref()
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .unwrap();

        assert_eq!(literal.value, 5);
        assert_eq!(literal.token, Token::INT("5".to_string()));
    }

    #[test]
    fn test_prefix_expression() {
        let tests = vec![("!5", "!", 5), ("-15", "-", 15)];
        for tt in tests {
            let l = Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(
                program.is_ok(),
                "Expected parsing to succeed. Error: {:?}",
                program.err()
            );
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
            let literal = exp
                .expression
                .as_ref()
                .as_any()
                .downcast_ref::<PrefixExpression>()
                .unwrap();

            assert_eq!(literal.token.text(), tt.1);

            test_integer_literal(&literal.right, tt.2);
        }
    }

    fn test_integer_literal(exp: &Box<dyn Expression>, value: i64) {
        let literal = exp.as_any().downcast_ref::<IntegerLiteral>().unwrap();

        assert_eq!(literal.value, value);
        assert_eq!(literal.token, Token::INT(value.to_string()));
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];
        for tt in tests {
            let l = Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(
                program.is_ok(),
                "Expected parsing to succeed. Error: {:?}",
                program.err()
            );
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
            test_infix_expression(&exp.expression, &tt.1, tt.2, &tt.3);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(
                program.is_ok(),
                "Expected parsing to succeed. Error: {:?}",
                program.err()
            );
            let program = program.unwrap();
            let actual = program.text();
            assert_eq!(actual, tt.1);
        }
    }

    fn test_literal_expression(exp: &Box<dyn Expression>, expected: &dyn Any) {
        if let Some(expected) = expected.downcast_ref::<i64>() {
            test_integer_literal(exp, *expected);
        } else if let Some(expected) = expected.downcast_ref::<i32>() {
            test_integer_literal(exp, *expected as i64);
        } else if let Some(expected) = expected.downcast_ref::<bool>() {
            test_boolean_literal(exp, *expected);
        } else if let Some(expected) = expected.downcast_ref::<&str>() {
            test_identifier(exp, expected);
        } else {
            panic!("type of exp not handled. got={}", exp.text());
        }
    }

    fn test_boolean_literal(exp: &Box<dyn Expression>, value: bool) {
        let boolean = exp.as_any().downcast_ref::<BooleanExpression>().unwrap();
        assert_eq!(boolean.token.text(), value.to_string());
    }

    fn test_infix_expression(
        exp: &Box<dyn Expression>,
        left: &dyn Any,
        operator: &str,
        right: &dyn Any,
    ) {
        let op_exp = exp.as_any().downcast_ref::<InfixExpression>().unwrap();
        test_literal_expression(&op_exp.left, left);
        assert_eq!(op_exp.token.text(), operator);
        test_literal_expression(&op_exp.right, right);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];
        for tt in tests {
            let l = Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            assert!(
                program.is_ok(),
                "Expected parsing to succeed. Error: {:?}",
                program.err()
            );
            let program = program.unwrap();
            assert_eq!(program.statements.len(), 1);
            let stmt = &program.statements[0];
            let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
            test_boolean_literal(&exp.expression, tt.1);
        }
    }
}
