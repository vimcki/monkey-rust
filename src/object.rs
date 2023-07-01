pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait Object {
    fn as_any(&self) -> &dyn std::any::Any;
    fn typee(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn typee(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn typee(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Null {}

impl Object for Null {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn typee(&self) -> ObjectType {
        ObjectType::Null
    }
    fn inspect(&self) -> String {
        return String::from("null");
    }
}
