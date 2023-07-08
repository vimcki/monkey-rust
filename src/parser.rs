use crate::{
    ast::{Expression, Identifier, Precedence, Prefix, Program, Statement},
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
        t: Token,
    ) -> Option<fn(&mut Parser) -> Result<Expression, String>> {
        match t {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer_literal),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix_expression),
            _ => None,
        }
    }

    fn get_infix_parse_fn(&self, t: Token) -> Option<fn(Expression) -> Expression> {
        match t {
            _ => None,
        }
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
        let prefix = self.get_prefix_parse_fn(self.cur_token.clone());
        if prefix.is_none() {
            return Err("parse_expression: no prefix parse function for token".to_string());
        }
        let mut left_exp = prefix.unwrap()(self);
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
        return Ok(Expression::IntegerLiteralExpression(value));
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
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Prefix, Statement};

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
                crate::ast::Expression::IdentifierExpression(ident) => {
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
                crate::ast::Expression::IntegerLiteralExpression(int) => {
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
        let prefix_tests = vec![("!5", Prefix::Bang, 5), ("-15", Prefix::Minus, 15)];
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
                    crate::ast::Expression::PrefixExpression(prefix, inner_expr) => {
                        if *prefix != tt.1 {
                            panic!("prefix.operator is not {:?}. got={:?}", tt.1, prefix);
                        }
                        tes_integer_literal(inner_expr, tt.2);
                    }
                    _ => panic!("stmt.expression not PrefixExpression. got={:?}", stmt),
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", stmt),
            }
        }
    }

    fn tes_integer_literal(expression: &Expression, value: i64) {
        match expression {
            Expression::IntegerLiteralExpression(int) => {
                if *int != value {
                    panic!("int.value not {}. got={}", value, int);
                }
            }
            _ => panic!("expression not IntegerLiteral. got={:?}", expression),
        }
    }
}
