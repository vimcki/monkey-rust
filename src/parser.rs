use crate::{
    ast::{Expression, Identifier, Infix, Literal, Precedence, Prefix, Program, Statement},
    lexer::lexer::{Lexer, Token},
};

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
        };

        p.next_token();
        p.next_token();

        return p;
    }

    fn get_prefix_parse_fn(
        &self,
        t: &Token,
    ) -> Option<fn(&mut Parser) -> Result<Expression, String>> {
        match t {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer_literal),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix_expression),
            Token::TRUE | Token::FALSE => Some(Parser::parse_boolean),
            Token::LPAREN => Some(Parser::parse_grouped_expression),
            Token::IF => Some(Parser::parse_if_expression),
            Token::FUNCTION => Some(Parser::parse_function_expression),
            _ => None,
        }
    }

    fn get_infix_parse_fn(
        &self,
        t: &Token,
    ) -> Option<fn(&mut Parser, Expression) -> Result<Expression, String>> {
        match t {
            Token::PLUS => Some(Parser::parse_infix_expression),
            Token::MINUS => Some(Parser::parse_infix_expression),
            Token::SLASH => Some(Parser::parse_infix_expression),
            Token::ASTERISK => Some(Parser::parse_infix_expression),
            Token::EQUAL => Some(Parser::parse_infix_expression),
            Token::NOTEQUAL => Some(Parser::parse_infix_expression),
            Token::LT => Some(Parser::parse_infix_expression),
            Token::GT => Some(Parser::parse_infix_expression),
            Token::LPAREN => Some(Parser::parse_call_expression),
            _ => None,
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let arguments = self.parse_call_arguments()?;
        return Ok(Expression::CallExpression {
            function: Box::new(left),
            arguments,
        });
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let operator = match self.cur_token.clone() {
            Token::PLUS => Infix::Plus,
            Token::MINUS => Infix::Minus,
            Token::SLASH => Infix::Slash,
            Token::ASTERISK => Infix::Asterisk,
            Token::EQUAL => Infix::Eq,
            Token::NOTEQUAL => Infix::NotEq,
            Token::LT => Infix::Lt,
            Token::GT => Infix::Gt,
            _ => return Err("parse_infix_expression: cur_token is not PLUS".to_string()),
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        return Ok(Expression::InfixExpression(
            Box::new(left),
            operator,
            Box::new(right),
        ));
    }

    fn get_precedence(&self, t: &Token) -> Precedence {
        match t {
            Token::EQUAL => Precedence::Equals,
            Token::NOTEQUAL => Precedence::Equals,
            Token::LT => Precedence::LessGreater,
            Token::GT => Precedence::LessGreater,
            Token::PLUS => Precedence::Sum,
            Token::MINUS => Precedence::Sum,
            Token::SLASH => Precedence::Product,
            Token::ASTERISK => Precedence::Product,
            Token::LPAREN => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        return self.get_precedence(&self.peek_token);
    }

    fn cur_precedence(&self) -> Precedence {
        return self.get_precedence(&self.cur_token);
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        Ok(Expression::IdentifierExpression(Identifier {
            name: self.cur_token.text(),
        }))
    }

    pub fn next_token(&mut self) {
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

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        if !self.expect_peek(Token::IDENT("".to_string())) {
            return Err("parse_let_statement: expect_peek failed".to_string());
        }
        let name = match self.cur_token {
            Token::IDENT(ref s) => s.clone(),
            _ => return Err("parse_let_statement: cur_token is not IDENT".to_string()),
        };
        if !self.expect_peek(Token::ASSIGN) {
            return Err("parse_let_statement: expect_peek failed".to_string());
        }
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        let ident = Identifier { name };
        let expr = Expression::IdentifierExpression(Identifier {
            name: "".to_string(),
        });
        return Ok(Statement::LetStatement(ident, expr));
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

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        while !self.cur_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        return Ok(Statement::ReturnStatement(
            Expression::IdentifierExpression(Identifier {
                name: "".to_string(),
            }),
        ));
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        return Ok(Statement::ExpressionStatement(expr));
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn parse_expression(&mut self, lowest: Precedence) -> Result<Expression, String> {
        let mut left_exp = match self.get_prefix_parse_fn(&self.cur_token) {
            Some(prefix) => prefix(self),
            None => {
                return Err(format!(
                    "parse_expression: no prefix parse function for token {:?}",
                    self.cur_token
                ))
            }
        };

        while !self.peek_token_is(Token::SEMICOLON) && lowest < self.peek_precedence() {
            match self.get_infix_parse_fn(&self.peek_token) {
                Some(infix) => {
                    self.next_token();
                    left_exp = infix(self, left_exp.unwrap());
                }
                None => return left_exp,
            };
        }
        return left_exp;
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        let value = match self.cur_token {
            Token::INT(ref s) => s.clone(),
            _ => {
                return Err(format!(
                    "parse_integer_literal: cur_token is not INT. got={:?}",
                    self.cur_token
                ))
            }
        };
        let value = value.parse::<i64>().unwrap();
        return Ok(Expression::LiteralExpression(Literal::IntegerLiteral(
            value,
        )));
    }

    fn parse_boolean(&mut self) -> Result<Expression, String> {
        return match self.cur_token {
            Token::TRUE => Ok(Expression::LiteralExpression(Literal::BooleanLiteral(true))),
            Token::FALSE => Ok(Expression::LiteralExpression(Literal::BooleanLiteral(
                false,
            ))),
            _ => Err(format!(
                "parse_boolean: cur_token is not TRUE or FALSE. got={:?}",
                self.cur_token
            )),
        };
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, String> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_grouped_expression: expect_peek failed".to_string());
        }
        return exp;
    }

    fn parse_function_expression(&mut self) -> Result<Expression, String> {
        if !self.expect_peek(Token::LPAREN) {
            return Err("parse_function_expression: expect_peek failed".to_string());
        }
        let parameters = self.parse_function_parameters()?;
        if !self.expect_peek(Token::LBRACE) {
            return Err("parse_function_expression: expect_peek failed".to_string());
        }
        let body = Box::new(self.parse_block_statement()?);
        return Ok(Expression::FunctionExpression { parameters, body });
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, String> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        let ident = Identifier {
            name: match self.cur_token {
                Token::IDENT(ref s) => s.clone(),
                _ => return Err("parse_function_parameters: cur_token is not IDENT".to_string()),
            },
        };
        identifiers.push(ident);
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                name: match self.cur_token {
                    Token::IDENT(ref s) => s.clone(),
                    _ => {
                        return Err("parse_function_parameters: cur_token is not IDENT".to_string())
                    }
                },
            };
            identifiers.push(ident);
        }
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_function_parameters: expect_peek failed".to_string());
        }
        return Ok(identifiers);
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        if !self.expect_peek(Token::LPAREN) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        if !self.expect_peek(Token::LBRACE) {
            return Err("parse_if_expression: expect_peek failed".to_string());
        }
        let consequence = self.parse_block_statement()?;
        let alternative = match self.peek_token {
            Token::ELSE => {
                self.next_token();
                if !self.expect_peek(Token::LBRACE) {
                    return Err("parse_if_expression: expect_peek failed".to_string());
                }
                Some(self.parse_block_statement()?)
            }
            _ => None,
        };
        return Ok(Expression::IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: alternative.map(|x| Box::new(x)),
        });
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let mut statements = Vec::new();
        self.next_token();
        while !self.cur_token_is(Token::RBRACE) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        return Ok(Statement::BlockStatement(statements));
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let operator = match self.cur_token {
            Token::BANG => Prefix::Bang,
            Token::MINUS => Prefix::Minus,
            _ => {
                return Err(format!(
                    "parse_prefix_expression: cur_token is not BANG or MINUS. got={:?}",
                    self.cur_token
                ))
            }
        };
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        return Ok(Expression::PrefixExpression(operator, Box::new(right)));
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();
        if self.peek_token_is(Token::RPAREN) {
            return Ok(args);
        }
        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }
        if !self.expect_peek(Token::RPAREN) {
            return Err("parse_call_arguments: expect_peek failed".to_string());
        }
        return Ok(args);
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{self, Expression, Identifier, Infix, Literal, Prefix, Statement};

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }
        let tests = vec!["x", "y", "foobar"];
        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, tt);
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        match stmt {
            Statement::LetStatement(stmt, _) => {
                if stmt.name != name {
                    panic!("stmt.name.value not '{}'. got={}", name, stmt.name);
                }
            }
            _ => panic!("stmt not LetStatement. got={:?}", stmt),
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }
        for stmt in program.statements {
            match stmt {
                Statement::ReturnStatement(_) => {}
                _ => panic!("stmt not ReturnStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );
        }
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement(stmt) => match stmt {
                ast::Expression::IdentifierExpression(ident) => {
                    if ident.name != "foobar" {
                        panic!("ident.name not {}. got={}", "foobar", ident.name);
                    }
                }
                _ => panic!("stmt.expression not Identifier. got={:?}", stmt),
            },
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );
        }
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement(stmt) => match stmt {
                ast::Expression::LiteralExpression(Literal::IntegerLiteral(int)) => {
                    if *int != 5 {
                        panic!("int.value not {}. got={}", 5, int);
                    }
                }
                _ => panic!("stmt.expression not IntegerLiteral. got={:?}", stmt),
            },
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = vec![
            ("!5", Prefix::Bang, Literal::IntegerLiteral(5)),
            ("-15", Prefix::Minus, Literal::IntegerLiteral(15)),
            ("!true", Prefix::Bang, Literal::BooleanLiteral(true)),
            ("!false", Prefix::Bang, Literal::BooleanLiteral(false)),
        ];
        for tt in prefix_tests {
            let input = tt.0;
            let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
            let mut p = super::Parser::new(l);
            let program = p.parse_program();
            if let Err(e) = program {
                panic!("parse_program: {}", e);
            }
            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements. got={}",
                    program.statements.len()
                );
            }
            let stmt = &program.statements[0];
            match stmt {
                Statement::ExpressionStatement(stmt) => match stmt {
                    ast::Expression::PrefixExpression(prefix, inner_expr) => {
                        if *prefix != tt.1 {
                            panic!("prefix.operator is not {:?}. got={:?}", tt.1, prefix);
                        }
                        test_literal_expression(inner_expr, &tt.2);
                    }
                    _ => panic!("stmt.expression not PrefixExpression. got={:?}", stmt),
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
            }
        }
    }

    fn tes_integer_literal(expression: &Expression, value: i64) {
        match expression {
            Expression::LiteralExpression(Literal::IntegerLiteral(int)) => {
                if *int != value {
                    panic!("int.value not {}. got={}", value, int);
                }
            }
            _ => panic!("expression not IntegerLiteral. got={:?}", expression),
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            (
                "5 + 5;",
                Literal::IntegerLiteral(5),
                Infix::Plus,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 - 5;",
                Literal::IntegerLiteral(5),
                Infix::Minus,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 * 5;",
                Literal::IntegerLiteral(5),
                Infix::Asterisk,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 / 5;",
                Literal::IntegerLiteral(5),
                Infix::Slash,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 > 5;",
                Literal::IntegerLiteral(5),
                Infix::Gt,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 < 5;",
                Literal::IntegerLiteral(5),
                Infix::Lt,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 == 5;",
                Literal::IntegerLiteral(5),
                Infix::Eq,
                Literal::IntegerLiteral(5),
            ),
            (
                "5 != 5;",
                Literal::IntegerLiteral(5),
                Infix::NotEq,
                Literal::IntegerLiteral(5),
            ),
            (
                "true == true",
                Literal::BooleanLiteral(true),
                Infix::Eq,
                Literal::BooleanLiteral(true),
            ),
            (
                "true != false",
                Literal::BooleanLiteral(true),
                Infix::NotEq,
                Literal::BooleanLiteral(false),
            ),
            (
                "false == false",
                Literal::BooleanLiteral(false),
                Infix::Eq,
                Literal::BooleanLiteral(false),
            ),
        ];
        for tt in infix_tests {
            let input = tt.0;
            let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
            let mut p = super::Parser::new(l);
            let program = p.parse_program();
            if let Err(e) = program {
                panic!("parse_program: {}", e);
            }
            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements. got={}",
                    program.statements.len()
                );
            }
            let stmt = &program.statements[0];
            match stmt {
                Statement::ExpressionStatement(stmt) => match stmt {
                    ast::Expression::InfixExpression(left, infix, right) => {
                        test_literal_expression(left, &tt.1);
                        if *infix != tt.2 {
                            panic!("infix.operator is not {:?}. got={:?}", tt.2, infix);
                        }
                        test_literal_expression(right, &tt.3);
                    }
                    _ => panic!("stmt.expression not InfixExpression. got={:?}", stmt),
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
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
            let l = crate::lexer::lexer::Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = super::Parser::new(l);
            let program = p.parse_program();
            if let Err(e) = program {
                panic!("parse_program: {}", e);
            }
            let program = program.unwrap();
            let actual = program.text();
            if actual != tt.1 {
                panic!("expected={}, got={}", tt.1, actual);
            }
        }
    }

    fn test_identifier(expression: &Expression, value: &str) {
        match expression {
            Expression::IdentifierExpression(ident) => {
                if ident.name != value {
                    panic!("ident.value not {}. got={}", value, ident.name);
                }
            }
            _ => panic!("expression not Identifier. got={:?}", expression),
        }
    }

    fn test_literal_expression(expression: &Expression, expected: &Literal) {
        match expression {
            Expression::LiteralExpression(literal) => match literal {
                Literal::IntegerLiteral(int) => {
                    tes_integer_literal(expression, *int);
                }
                Literal::BooleanLiteral(boolean) => {
                    test_boolean_literal(expression, *boolean);
                }
                Literal::StringLiteral(string) => {
                    test_string_literal(expression, string);
                }
            },
            _ => panic!("expression not Literal. got={:?}", expression),
        }
    }

    fn test_boolean_literal(expression: &Expression, value: bool) {
        match expression {
            Expression::LiteralExpression(Literal::BooleanLiteral(boolean)) => {
                if *boolean != value {
                    panic!("boolean.value not {}. got={}", value, boolean);
                }
            }
            _ => panic!("expression not BooleanLiteral. got={:?}", expression),
        }
    }

    fn test_string_literal(expression: &Expression, value: &str) {
        match expression {
            Expression::LiteralExpression(Literal::StringLiteral(string)) => {
                if *string != value {
                    panic!("string.value not {}. got={}", value, string);
                }
            }
            _ => panic!("expression not StringLiteral. got={:?}", expression),
        }
    }

    fn test_infix_expression(
        expression: &Expression,
        left_lit: &Literal,
        want_operator: &Infix,
        right_lit: &Literal,
    ) {
        match expression {
            Expression::InfixExpression(left, operator, right) => {
                test_literal_expression(left, left_lit);
                if *want_operator != *operator {
                    panic!(
                        "expression.operator is not {:?}. got={:?}",
                        operator, operator
                    );
                }
                test_literal_expression(right, right_lit);
            }
            _ => panic!("expression not InfixExpression. got={:?}", expression),
        }
    }

    #[test]
    fn test_if_expression() {
        let input = vec!["if (x < y) { x }", "if (x < y) { x } else { y }"];
        let expected_condition = vec![
            (Expression::InfixExpression(
                Box::new(Expression::IdentifierExpression(Identifier {
                    name: "x".to_string(),
                })),
                Infix::Lt,
                Box::new(Expression::IdentifierExpression(Identifier {
                    name: "y".to_string(),
                })),
            )),
            (Expression::InfixExpression(
                Box::new(Expression::IdentifierExpression(Identifier {
                    name: "x".to_string(),
                })),
                Infix::Lt,
                Box::new(Expression::IdentifierExpression(Identifier {
                    name: "y".to_string(),
                })),
            )),
        ];
        let expected_consequence = vec![
            (Statement::ExpressionStatement(Expression::IdentifierExpression(Identifier {
                name: "x".to_string(),
            }))),
            (Statement::ExpressionStatement(Expression::IdentifierExpression(Identifier {
                name: "x".to_string(),
            }))),
        ];
        let expected_alternative = vec![
            None,
            Some(Statement::ExpressionStatement(
                Expression::IdentifierExpression(Identifier {
                    name: "y".to_string(),
                }),
            )),
        ];

        for (i, tt) in input.iter().enumerate() {
            let l = crate::lexer::lexer::Lexer::new(tt.as_bytes().to_vec());
            let mut p = super::Parser::new(l);
            let program = p.parse_program();
            if let Err(e) = program {
                panic!("parse_program: {}", e);
            }
            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements. got={}",
                    program.statements.len()
                );
            }
            let stmt = &program.statements[0];
            match stmt {
                Statement::ExpressionStatement(expr) => match expr {
                    Expression::IfExpression {
                        condition,
                        consequence,
                        alternative,
                    } => {
                        if condition.text() != expected_condition[i].text() {
                            panic!(
                                "condition not {}. got={}",
                                expected_condition[i].text(),
                                condition.text()
                            );
                        }
                        if consequence.text() != expected_consequence[i].text() {
                            panic!(
                                "consequence not {}. got={}",
                                expected_consequence[i].text(),
                                consequence.text()
                            );
                        }
                        match alternative {
                            Some(alt) => {
                                if alt.text() != expected_alternative[i].as_ref().unwrap().text() {
                                    panic!(
                                        "alternative not {}. got={}",
                                        expected_alternative[i].as_ref().unwrap().text(),
                                        alt.text()
                                    );
                                }
                            }
                            None => {
                                if expected_alternative[i].is_some() {
                                    panic!(
                                        "alternative not {}. got={}",
                                        expected_alternative[i].as_ref().unwrap().text(),
                                        "None"
                                    );
                                }
                            }
                        }
                    }
                    _ => panic!("expression not IfExpression. got={:?}", expr),
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_function_expression() {
        let input = "fn(x, y) { x + y; }";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );
        }
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement(expr) => match expr {
                Expression::FunctionExpression { parameters, body } => {
                    if parameters.len() != 2 {
                        panic!(
                            "function literal parameters wrong. want 2, got={}",
                            parameters.len()
                        );
                    }
                    if parameters[0].text() != "x" {
                        panic!(
                            "parameter[0] wrong. want {}, got={}",
                            "x",
                            parameters[0].text()
                        );
                    }
                    if parameters[1].text() != "y" {
                        panic!(
                            "parameter[1] wrong. want {}, got={}",
                            "y",
                            parameters[1].text()
                        );
                    }
                    if body.text() != "(x + y)" {
                        panic!("body wrong. want {}, got={}", "{ x + y; }", body.text());
                    }
                }
                _ => panic!("expression not FunctionExpression. got={:?}", expr),
            },
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
        }
    }

    #[test]
    fn test_function_parameters() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];
        for tt in tests {
            let l = crate::lexer::lexer::Lexer::new(tt.0.as_bytes().to_vec());
            let mut p = super::Parser::new(l);
            let program = p.parse_program();
            if let Err(e) = program {
                panic!("parse_program: {}", e);
            }
            let program = program.unwrap();
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements. got={}",
                    program.statements.len()
                );
            }
            let stmt = &program.statements[0];
            match stmt {
                Statement::ExpressionStatement(expr) => match expr {
                    Expression::FunctionExpression { parameters, body } => {
                        if parameters.len() != tt.1.len() {
                            panic!(
                                "function literal parameters wrong. want {}, got={}",
                                tt.1.len(),
                                parameters.len()
                            );
                        }
                        for (i, ident) in parameters.iter().enumerate() {
                            if ident.text() != tt.1[i] {
                                panic!(
                                    "parameter[{}] wrong. want {}, got={}",
                                    i,
                                    tt.1[i],
                                    ident.text()
                                );
                            }
                        }
                        if body.text() != "" {
                            panic!("body wrong. want {}, got={}", "{}", body.text());
                        }
                    }
                    _ => panic!("expression not FunctionExpression. got={:?}", expr),
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let l = crate::lexer::lexer::Lexer::new(input.as_bytes().to_vec());
        let mut p = super::Parser::new(l);
        let program = p.parse_program();
        if let Err(e) = program {
            panic!("parse_program: {}", e);
        }
        let program = program.unwrap();
        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements. got={}",
                program.statements.len()
            );
        }
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement(expr) => match expr {
                Expression::CallExpression {
                    function,
                    arguments,
                } => {
                    if function.text() != "add" {
                        panic!(
                            "function identifier wrong. want {}, got={}",
                            "add",
                            function.text()
                        );
                    }
                    if arguments.len() != 3 {
                        panic!("arguments wrong. want {}, got={}", 3, arguments.len());
                    }
                    if arguments[0].text() != "1" {
                        panic!(
                            "arguments[0] wrong. want {}, got={}",
                            "1",
                            arguments[0].text()
                        );
                    }
                    if arguments[1].text() != "(2 * 3)" {
                        panic!(
                            "arguments[1] wrong. want {}, got={}",
                            "(2 * 3)",
                            arguments[1].text()
                        );
                    }
                    if arguments[2].text() != "(4 + 5)" {
                        panic!(
                            "arguments[2] wrong. want {}, got={}",
                            "(4 + 5)",
                            arguments[2].text()
                        );
                    }
                }
                _ => panic!("expression not CallExpression. got={:?}", expr),
            },
            _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
        }
    }
}
