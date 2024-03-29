#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LetStatement(Identifier, Expression),
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    BlockStatement(Vec<Statement>),
}

impl Statement {
    pub fn text(&self) -> String {
        match self {
            Statement::LetStatement(i, e) => format!("let {} = {};", i.name, e.text()),
            Statement::ReturnStatement(e) => format!("return {};", e.text()),
            Statement::ExpressionStatement(e) => format!("{}", e.text()),
            Statement::BlockStatement(s) => {
                let mut text = String::new();
                for s in s.iter() {
                    text.push_str(&s.text());
                }
                return text;
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    IdentifierExpression(Identifier),
    LiteralExpression(Literal),
    PrefixExpression(Prefix, Box<Expression>),
    InfixExpression(Box<Expression>, Infix, Box<Expression>),
    IfExpression {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    FunctionExpression {
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Expression {
    pub fn text(&self) -> String {
        match self {
            Expression::IdentifierExpression(i) => i.text(),
            Expression::LiteralExpression(l) => l.text(),
            Expression::PrefixExpression(op, e) => format!("({}{})", op.text(), e.text()),
            Expression::InfixExpression(l, op, r) => {
                format!("({} {} {})", l.text(), op.text(), r.text())
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let mut text = format!("if {} {}", condition.text(), consequence.text());
                if let Some(a) = alternative {
                    text.push_str(&format!("else {}", a.text()));
                }
                text
            }
            Expression::FunctionExpression { parameters, body } => {
                let mut text = String::new();
                text.push_str("fn(");
                for (i, p) in parameters.iter().enumerate() {
                    if i != 0 {
                        text.push_str(", ");
                    }
                    text.push_str(&p.text());
                }
                text.push_str(") ");
                text.push_str(&body.text());
                text
            }
            Expression::CallExpression {
                function,
                arguments,
            } => {
                let mut text = String::new();
                text.push_str(&function.text());
                text.push_str("(");
                for (i, a) in arguments.iter().enumerate() {
                    if i != 0 {
                        text.push_str(", ");
                    }
                    text.push_str(&a.text());
                }
                text.push_str(")");
                text
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
}

impl Literal {
    pub fn text(&self) -> String {
        match self {
            Literal::IntegerLiteral(i) => i.to_string(),
            Literal::BooleanLiteral(b) => b.to_string(),
            Literal::StringLiteral(s) => s.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn text(&self) -> String {
        return self.name.clone();
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Eq,
    NotEq,
    Lt,
    Gt,
}

impl Infix {
    pub fn text(&self) -> String {
        match self {
            Infix::Plus => "+".to_string(),
            Infix::Minus => "-".to_string(),
            Infix::Asterisk => "*".to_string(),
            Infix::Slash => "/".to_string(),
            Infix::Eq => "==".to_string(),
            Infix::NotEq => "!=".to_string(),
            Infix::Lt => "<".to_string(),
            Infix::Gt => ">".to_string(),
        }
    }
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
