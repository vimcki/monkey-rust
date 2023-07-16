use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }
    pub fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        let mut hashmap = HashMap::new();
        Environment {
            store: hashmap,
            parent: Some(outer),
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
