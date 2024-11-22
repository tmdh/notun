use std::{collections::HashMap, rc::Rc};

use crate::type_checker::{Type, ValueConstructor, ValueConstructorVariant};

pub struct Environment {
    pub scope: HashMap<String, ValueConstructor>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scope: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, variant: ValueConstructorVariant, type_: Rc<Type>) {
        self.scope.insert(name, ValueConstructor { variant, type_ });
    }

    pub fn insert_local_variable(&mut self, name: String, type_: Rc<Type>) {
        self.insert(name, ValueConstructorVariant::LocalVariable, type_);
    }

    pub fn get(&self, name: &String) -> Option<&ValueConstructor> {
        self.scope.get(name)
    }

    pub fn enter_new_scope<T>(
        &mut self,
        inside_scope_fn: impl FnOnce(&mut Self) -> Option<T>,
    ) -> Option<T> {
        let current_scope_hashmap = self.scope.clone();
        let result = inside_scope_fn(self);
        self.scope = current_scope_hashmap;
        result
    }
}
