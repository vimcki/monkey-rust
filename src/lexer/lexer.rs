#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    IF,
    RETURN,
    TRUE,
    FALSE,
    ELSE,
    EQUAL,
    NOTEQUAL,
}

impl Token {
    pub fn text(&self) -> String {
        return match self {
            Token::ILLEGAL => "ILLEGAL".to_string(),
            Token::EOF => "EOF".to_string(),
            Token::IDENT(s) => s.to_string(),
            Token::INT(s) => s.to_string(),
            Token::ASSIGN => "=".to_string(),
            Token::PLUS => "+".to_string(),
            Token::COMMA => ",".to_string(),
            Token::SEMICOLON => ";".to_string(),
            Token::LPAREN => "(".to_string(),
            Token::RPAREN => ")".to_string(),
            Token::LBRACE => "{".to_string(),
            Token::RBRACE => "}".to_string(),
            Token::FUNCTION => "fn".to_string(),
            Token::LET => "let".to_string(),
            Token::MINUS => "-".to_string(),
            Token::BANG => "!".to_string(),
            Token::ASTERISK => "*".to_string(),
            Token::SLASH => "/".to_string(),
            Token::LT => "<".to_string(),
            Token::GT => ">".to_string(),
            Token::IF => "if".to_string(),
            Token::RETURN => "return".to_string(),
            Token::TRUE => "true".to_string(),
            Token::FALSE => "false".to_string(),
            Token::ELSE => "else".to_string(),
            Token::EQUAL => "==".to_string(),
            Token::NOTEQUAL => "!=".to_string(),
        };
    }
}

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: Vec<u8>) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        return l;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;

        self.read_position += 1;
    }

    fn peek(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek() == '=' as u8 {
                    self.read_char();
                    Token::EQUAL
                } else {
                    Token::ASSIGN
                }
            }
            b'+' => Token::PLUS,
            b',' => Token::COMMA,
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b'-' => Token::MINUS,
            b'!' => {
                if self.peek() == '=' as u8 {
                    self.read_char();
                    Token::NOTEQUAL
                } else {
                    Token::BANG
                }
            }

            b'*' => Token::ASTERISK,
            b'/' => Token::SLASH,
            b'<' => Token::LT,
            b'>' => Token::GT,
            0 => Token::EOF,
            ch => {
                if is_letter(ch) {
                    return lookup_ident(self.read_identifier());
                } else if is_digit(ch) {
                    return Token::INT(self.read_number());
                }
                Token::ILLEGAL
            }
        };
        self.read_char();
        return tok;
    }
    fn read_identifier(&mut self) -> String {
        let starting_pos = self.position;
        while is_letter(self.ch) {
            self.read_char()
        }
        return String::from_utf8_lossy(&self.input[starting_pos..self.position]).into_owned();
    }

    fn read_number(&mut self) -> String {
        let starting_pos = self.position;
        while is_digit(self.ch) {
            self.read_char()
        }
        return String::from_utf8_lossy(&self.input[starting_pos..self.position]).into_owned();
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char()
        }
    }
}

fn lookup_ident(ident: String) -> Token {
    let token = match ident.as_str() {
        "fn" => Token::FUNCTION,
        "let" => Token::LET,
        "if" => Token::IF,
        "true" => Token::TRUE,
        "false" => Token::FALSE,
        "else" => Token::ELSE,
        "return" => Token::RETURN,
        _ => Token::IDENT(ident),
    };
    return token;
}

fn is_letter(ch: u8) -> bool {
    return 'a' as u8 <= ch && ch <= 'z' as u8
        || 'A' as u8 <= ch && ch <= 'Z' as u8
        || ch == '_' as u8;
}

fn is_digit(ch: u8) -> bool {
    return '0' as u8 <= ch && ch <= '9' as u8;
}

#[test]
fn test_lexer() {
    let input = "=+(){},;";
    let tests = vec![
        Token::ASSIGN,
        Token::PLUS,
        Token::LPAREN,
        Token::RPAREN,
        Token::LBRACE,
        Token::RBRACE,
        Token::COMMA,
        Token::SEMICOLON,
        Token::EOF,
    ];
    let mut l = Lexer::new(input.into());
    for want in tests {
        let got = l.next_token();
        assert_eq!(got, want);
    }
}

#[test]
fn test_lexer2() {
    let input = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        "#;

    let tokens = vec![
        Token::LET,
        Token::IDENT(String::from("five")),
        Token::ASSIGN,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("ten")),
        Token::ASSIGN,
        Token::INT(String::from("10")),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("add")),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT(String::from("x")),
        Token::COMMA,
        Token::IDENT(String::from("y")),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT(String::from("x")),
        Token::PLUS,
        Token::IDENT(String::from("y")),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT(String::from("result")),
        Token::ASSIGN,
        Token::IDENT(String::from("add")),
        Token::LPAREN,
        Token::IDENT(String::from("five")),
        Token::COMMA,
        Token::IDENT(String::from("ten")),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERISK,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::INT(String::from("5")),
        Token::LT,
        Token::INT(String::from("10")),
        Token::GT,
        Token::INT(String::from("5")),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT(String::from("5")),
        Token::LT,
        Token::INT(String::from("10")),
        Token::RPAREN,
        Token::LBRACE,
        Token::RETURN,
        Token::TRUE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::ELSE,
        Token::LBRACE,
        Token::RETURN,
        Token::FALSE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::INT(String::from("10")),
        Token::EQUAL,
        Token::INT(String::from("10")),
        Token::SEMICOLON,
        Token::INT(String::from("10")),
        Token::NOTEQUAL,
        Token::INT(String::from("9")),
        Token::SEMICOLON,
        Token::EOF,
    ];
    let mut l = Lexer::new(input.into());
    for want in tokens.iter() {
        let got = l.next_token();
        assert_eq!(got, *want);
    }
}
