use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program,
        ReturnStatement, Statement,
    },
    lexer::lexer::{Lexer, Token},
};

enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

type prefix_parse_fn = fn(&mut Parser) -> Result<Box<dyn Expression>, String>;
type infix_parse_fn = fn(&mut Parser, Box<dyn Statement>) -> Result<Box<dyn Expression>, String>;

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

    fn get_prefix_fn(&self, t: Token) -> Result<prefix_parse_fn, String> {
        return match t {
            Token::IDENT(_) => Ok(Parser::parse_identifier),
            Token::INT(_) => Ok(Parser::parse_integer_literal),
            _ => Err("get_prefix_fn: no prefix parse function for token".to_string()),
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
        let left_exp = func(self)?;
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
}

#[cfg(test)]
mod tests {
    use crate::ast::ExpressionStatement;
    use crate::ast::Identifier;
    use crate::ast::IntegerLiteral;
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
        let ident = exp
            .expression
            .as_ref()
            .as_any()
            .downcast_ref::<Identifier>()
            .unwrap();

        let literal = match ident.token {
            Token::IDENT(ref s) => s,
            _ => panic!("not ident"),
        };

        assert_eq!(literal, "foobar");
    }

    #[test]
    fn test_integer_literal() {
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
}
