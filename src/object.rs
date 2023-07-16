#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => String::from("null"),
            Object::ReturnValue(obj) => obj.inspect(),
        }
    }
}
