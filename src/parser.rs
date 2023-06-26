use std::collections::HashMap;

use crate::{
    ast::{
        BlockStatement, BooleanExpression, CallExpression, Expression, ExpressionStatement,
        FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
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
        p.precedences.insert(Token::LPAREN, Precedence::CALL);

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
            Token::LPAREN => Ok(Parser::parse_grouped_expression),
            Token::IF => Ok(Parser::parse_if_expression),
            Token::FUNCTION => Ok(Parser::parse_function_literal),
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
            Token::LPAREN => Ok(Parser::parse_call_expression),
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

    fn parse_grouped_expression(&mut self) -> Result<Box<dyn Expression>, String> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_grouped_expression: expect_peek failed".to_string());
        }
        return Ok(exp);
    }

    fn parse_if_expression(&mut self) -> Result<Box<dyn Expression>, String> {
        if !self.expect_peek(Token::LPAREN) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        if !self.expect_peek(Token::LBRACE) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;
        if self.peek_token_is(Token::ELSE) {
            self.next_token();
            if !self.expect_peek(Token::LBRACE) {
                return Err("parse_if_expression: expect_peek failed".to_string());
            }
            alternative = Some(self.parse_block_statement()?);
        }
        return Ok(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        }));
    }

    fn parse_function_literal(&mut self) -> Result<Box<dyn Expression>, String> {
        if !self.expect_peek(Token::LPAREN) {
            return Err("parse_function_literal: expect_peek failed".to_string());
        }
        let parameters = self.parse_function_parameters()?;
        if !self.expect_peek(Token::LBRACE) {
            return Err("parse_function_literal: expect_peek failed".to_string());
        }
        let body = self.parse_block_statement()?;
        return Ok(Box::new(FunctionLiteral { parameters, body }));
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Box<dyn Expression>>, String> {
        let mut identifiers = vec![];
        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        let ident = self.parse_identifier()?;
        identifiers.push(ident);
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = self.parse_identifier()?;
            identifiers.push(ident);
        }
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_function_parameters: expect_peek failed".to_string());
        }
        return Ok(identifiers);
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, String> {
        self.next_token();
        let mut statements = vec![];
        while !self.cur_token_is(Token::RBRACE) && !self.cur_token_is(Token::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        return Ok(BlockStatement { statements });
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

    fn parse_call_expression(
        &mut self,
        function: Box<dyn Expression>,
    ) -> Result<Box<dyn Expression>, String> {
        let arguments = self.parse_call_arguments()?;
        return Ok(Box::new(CallExpression {
            function,
            arguments,
        }));
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Box<dyn Expression>>, String> {
        let mut args = vec![];
        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Ok(args);
        }
        self.next_token();
        let arg = self.parse_expression(Precedence::LOWEST)?;
        args.push(arg);
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let arg = self.parse_expression(Precedence::LOWEST)?;
            args.push(arg);
        }
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_call_arguments: expect_peek failed".to_string());
        }
        return Ok(args);
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::ast::BooleanExpression;
    use crate::ast::CallExpression;
    use crate::ast::Expression;
    use crate::ast::ExpressionStatement;
    use crate::ast::FunctionLiteral;
    use crate::ast::Identifier;
    use crate::ast::IfExpression;
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

        let bool_tests = vec![("!true", "!", true), ("!false", "!", false)];

        for tt in bool_tests {
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

            test_boolean_literal(&literal.right, tt.2);
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

        let bool_tests = vec![
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
        ];

        for tt in bool_tests {
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
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
        let if_exp = exp
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();
        test_infix_expression(&if_exp.condition, &"x", "<", &"y");
        assert_eq!(if_exp.consequence.statements.len(), 1);
        let consequence = &if_exp.consequence.statements[0];
        test_identifier(
            &consequence
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression,
            &"x",
        );
        assert!(if_exp.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
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
        let if_exp = exp
            .expression
            .as_any()
            .downcast_ref::<IfExpression>()
            .unwrap();
        test_infix_expression(&if_exp.condition, &"x", "<", &"y");
        assert_eq!(if_exp.consequence.statements.len(), 1);
        let consequence = &if_exp.consequence.statements[0];
        test_identifier(
            &consequence
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression,
            &"x",
        );
        assert!(if_exp.alternative.is_some());
        let alternative = if_exp.alternative.as_ref().unwrap();
        assert_eq!(alternative.statements.len(), 1);
        let alternative = &alternative.statements[0];
        test_identifier(
            &alternative
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression,
            &"y",
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }";
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
        let func = exp
            .expression
            .as_any()
            .downcast_ref::<FunctionLiteral>()
            .unwrap();
        assert_eq!(func.parameters.len(), 2);
        test_literal_expression(&func.parameters[0], &"x");
        test_literal_expression(&func.parameters[1], &"y");
        assert_eq!(func.body.statements.len(), 1);
        let body_stmt = &func.body.statements[0];
        test_infix_expression(
            &body_stmt
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap()
                .expression,
            &"x",
            "+",
            &"y",
        );
    }

    #[test]
    fn test_fuction_parameter_parsing() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
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
            let stmt = &program.statements[0];
            let exp = stmt.as_any().downcast_ref::<ExpressionStatement>().unwrap();
            let func = exp
                .expression
                .as_any()
                .downcast_ref::<FunctionLiteral>()
                .unwrap();
            assert_eq!(func.parameters.len(), tt.1.len());
            for (i, ident) in tt.1.iter().enumerate() {
                test_literal_expression(&func.parameters[i], ident);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
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
        let call = exp
            .expression
            .as_any()
            .downcast_ref::<CallExpression>()
            .unwrap();
        test_identifier(&call.function, &"add");
        assert_eq!(call.arguments.len(), 3);
        test_literal_expression(&call.arguments[0], &1);
        test_infix_expression(&call.arguments[1], &2, "*", &3);
        test_infix_expression(&call.arguments[2], &4, "+", &5);
    }
}
