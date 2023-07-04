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
    TODO,
}

impl Expression {
    pub fn text(&self) -> String {
        return String::new();
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}
