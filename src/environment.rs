use crate::lox_value::LoxValue;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// This is where all the variables live
pub struct Environment {
    /// The scope enclosing this one will have its own environment
    /// This could just have lifetimes passed in since they'll all be owned by the interpreter but
    /// this route is easier for now. This is single-threaded anyways.
    enclosing: Option<Rc<RefCell<Environment>>>,
    /// variable names to values
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn new_sharable() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment::new()))
    }
    pub fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }))
    }

    // Allows redefinition of a variable in a single scope
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Option<LoxValue> {
        match self.values.get(name) {
            Some(v) => Some(v.clone()),
            // Check if the enclosing env has the value
            None => self
                .enclosing
                .as_ref()
                .and_then(|env| env.borrow().get(name)),
        }
    }

    /// returns true if assign completed, false if the variable was not found
    pub fn assign(&mut self, name: &str, value: LoxValue) -> bool {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            true
        } else {
            match &self.enclosing {
                Some(env) => env.borrow_mut().assign(name, value),
                None => false,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;

    #[test]
    fn set_and_get() {
        let shared_env = Environment::new_sharable();
        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);

        shared_env
            .borrow_mut()
            .define(&token.lexeme, LoxValue::Bool(true));

        let value = shared_env.borrow().get(&token.lexeme);
        assert!(value.is_some());

        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);
    }
}
