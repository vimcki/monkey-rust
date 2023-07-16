use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, val: Object) -> Option<Object> {
        self.store.insert(name, val)
    }
}
