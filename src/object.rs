#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => String::from("null"),
            Object::ReturnValue(obj) => obj.inspect(),
            Object::Error(s) => format!("ERROR: {}", s),
        }
    }

    pub fn typee(&self) -> String {
        match self {
            Object::Integer(_) => String::from("INTEGER"),
            Object::Boolean(_) => String::from("BOOLEAN"),
            Object::Null => String::from("NULL"),
            Object::ReturnValue(obj) => obj.typee(),
            Object::Error(_) => String::from("ERROR"),
        }
    }
}
