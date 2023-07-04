use crate::{
    ast::{Expression, Identifier, Program, Statement},
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
        let expr = Expression::TODO {};
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

    fn parse_return_statement(&self) -> Result<Statement, String> {
        todo!()
    }

    fn parse_expression_statement(&self) -> Result<Statement, String> {
        todo!()
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

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
}
