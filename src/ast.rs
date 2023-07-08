pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn text(&self) -> String {
        let mut text = String::new();
        for s in self.statements.iter() {
            text.push_str(&s.text());
        }
        return text;
    }
}

#[derive(Debug)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
}

impl Statement {
    pub fn text(&self) -> String {
        match self {
            Statement::LetStatement(i, e) => format!("let {} = {};", i.name, e.text()),
            Statement::ReturnStatement(e) => format!("return {};", e.text()),
            Statement::ExpressionStatement(e) => format!("{};", e.text()),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    IdentifierExpression(Identifier),
    IntegerLiteralExpression(i64),
    PrefixExpression(Prefix, Box<Expression>),
}

impl Expression {
    pub fn text(&self) -> String {
        match self {
            Expression::IdentifierExpression(i) => i.text(),
            Expression::IntegerLiteralExpression(i) => i.to_string(),
            Expression::PrefixExpression(op, e) => format!("({}{})", op.text(), e.text()),
            _ => "TODO".to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Bang,
    Minus,
}

impl Prefix {
    pub fn text(&self) -> String {
        match self {
            Prefix::Bang => "!".to_string(),
            Prefix::Minus => "-".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn text(&self) -> String {
        return self.name.clone();
    }
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[test]
fn test_text() {
    let program = Program {
        statements: vec![
            Statement::LetStatement(
                Identifier {
                    name: "myVar".to_string(),
                },
                Expression::IdentifierExpression(Identifier {
                    name: "aaa".to_string(),
                }),
            ),
            Statement::ReturnStatement(Expression::IdentifierExpression(Identifier {
                name: "myVar".to_string(),
            })),
        ],
    };
    assert_eq!(program.text(), "let myVar = aaa;return myVar;");
}
