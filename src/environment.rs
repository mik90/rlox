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

    fn ancestor(
        env: Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Option<Rc<RefCell<Environment>>> {
        let mut cur_env = env;

        // hop up the enclosing chain for each 'distance'
        for _ in 0..=distance {
            // TODO holy fuck this is jank, all this Rc<RefCell> crap needs to go away
            // Using rust instead of java verbatim does this
            let enclosing = cur_env.borrow().enclosing.clone();
            if let Some(enclosing) = enclosing {
                cur_env = enclosing.clone();
            }
        }
        Some(cur_env)
    }

    pub fn get_at(env: Rc<RefCell<Environment>>, distance: usize, name: &str) -> Option<LoxValue> {
        match Environment::ancestor(env, distance) {
            Some(ancestor) => ancestor.borrow().values.get(name).map(|v| v.clone()),
            None => None,
        }
    }

    pub fn assign_at(env: Rc<RefCell<Environment>>, distance: usize, name: &str, value: LoxValue) {
        // TODO handle the case where there's no ancestor
        Environment::ancestor(env, distance)
            .and_then(|env| env.borrow_mut().values.insert(name.to_string(), value));
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

    #[test]
    fn get_from_enclosing() {
        let global_env = Environment::new_sharable();

        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);
        global_env
            .borrow_mut()
            .define(&token.lexeme, LoxValue::Bool(true));

        // Create an env that is enclosed by the global env
        let local_env = Environment::new_with_enclosing(global_env.clone());
        let value = local_env.borrow().get(&token.lexeme);
        assert!(
            value.is_some(),
            "A value in the local env should be able to access one from the base env"
        );

        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);
    }
}
