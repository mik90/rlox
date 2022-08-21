use crate::lox_value::LoxValue;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

/// This is where all the variables live
#[derive(Clone)]
pub struct Environment {
    /// variable names to values
    values: HashMap<String, LoxValue>,
    pub enclosing: Option<Arc<Mutex<Environment>>>,
}

pub fn ancestor_of(
    mut env: Arc<Mutex<Environment>>,
    distance: usize,
) -> Option<Arc<Mutex<Environment>>> {
    if distance == 0 {
        return Some(env);
    }

    for _ in 0..distance {
        if env.lock().unwrap().enclosing.is_some() {
            // Iterate through the enclosing environments
            let enc = env.lock().unwrap().enclosing.as_ref().unwrap().clone();
            env = enc;
        } else {
            return None;
        };
    }
    Some(env)
}

pub fn get_copy_at(env: Arc<Mutex<Environment>>, distance: usize, name: &str) -> Option<LoxValue> {
    match ancestor_of(env, distance) {
        Some(env) => env.lock().unwrap().get_copy(name),
        None => None,
    }
}

pub fn assign_at(
    env: Arc<Mutex<Environment>>,
    distance: usize,
    name: &str,
    value: LoxValue,
) -> bool {
    match ancestor_of(env, distance) {
        Some(env) => env.lock().unwrap().assign(name, value),
        None => false,
    }
}

impl Environment {
    pub fn new_empty() -> Arc<Mutex<Environment>> {
        Arc::new(Mutex::new(Environment {
            values: HashMap::new(),
            enclosing: None,
        }))
    }
    pub fn new_enclosing(enclosing: Arc<Mutex<Environment>>) -> Arc<Mutex<Environment>> {
        Arc::new(Mutex::new(Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }))
    }

    // Allows redefinition of a variable in a single scope
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }
    pub fn get_copy(&self, name: &str) -> Option<LoxValue> {
        if let Some(v) = self.values.get(name) {
            Some(v.clone())
        } else {
            if let Some(enc) = self.enclosing.as_ref() {
                enc.lock().unwrap().get_copy(name)
            } else {
                None
            }
        }
    }

    /// return true if assignment suceeded, false if not
    pub fn assign(&mut self, name: &str, value: LoxValue) -> bool {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            return true;
        } else {
            // try enclosing
            if let Some(enc) = self.enclosing.as_ref() {
                enc.lock().unwrap().assign(name, value)
            } else {
                false
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
        let env = Environment::new_empty();
        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);

        env.lock()
            .unwrap()
            .define(&token.lexeme, LoxValue::Bool(true));

        let value = env.lock().unwrap().get_copy(&token.lexeme);
        assert!(value.is_some());

        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);
    }

    #[test]
    fn get_from_enclosing() {
        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);
        let globals = Environment::new_empty();
        globals
            .lock()
            .unwrap()
            .define(&token.lexeme, LoxValue::Bool(true));

        let env = Environment::new_enclosing(globals);

        let value = env.lock().unwrap().get_copy(&token.lexeme);
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);

        // Create an env that is enclosed by the global env
        let env_2 = Environment::new_enclosing(env);

        let value = env_2.lock().unwrap().get_copy(&token.lexeme);
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);
    }
}
