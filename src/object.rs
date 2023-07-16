use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Identifier, Program},
    environment::Environment,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Vec<Identifier>, Program, Rc<RefCell<Environment>>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => String::from("null"),
            Object::ReturnValue(obj) => obj.inspect(),
            Object::Error(s) => format!("ERROR: {}", s),
            Object::Function(params, body, _) => {
                let mut text = String::new();
                text.push_str("fn(");
                for (i, p) in params.iter().enumerate() {
                    if i != 0 {
                        text.push_str(", ");
                    }
                    text.push_str(&p.text());
                }
                text.push_str(") {\n");
                text.push_str(&body.text());
                text.push_str("\n}");
                text
            }
        }
    }

    pub fn typee(&self) -> String {
        match self {
            Object::Integer(_) => String::from("INTEGER"),
            Object::Boolean(_) => String::from("BOOLEAN"),
            Object::Null => String::from("NULL"),
            Object::ReturnValue(obj) => obj.typee(),
            Object::Error(_) => String::from("ERROR"),
            Object::Function(_, _, _) => String::from("FUNCTION"),
        }
    }
}
