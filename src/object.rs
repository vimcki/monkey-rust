#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => String::from("null"),
        }
    }
}
