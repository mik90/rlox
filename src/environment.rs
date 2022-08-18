use crate::lox_value::LoxValue;
use std::collections::HashMap;

/// This is where all the variables live
#[derive(Clone)]
pub struct Environment {
    /// variable names to values
    values: HashMap<String, LoxValue>,
}

/// Owns all of the different environments/scopes
#[derive(Clone)]
pub struct EnvironmentStack {
    /// The first element is the bottom of the stack, the last is the top
    envs: Vec<Environment>,
}

impl EnvironmentStack {
    pub fn new(globals: Environment) -> EnvironmentStack {
        EnvironmentStack {
            envs: vec![globals],
        }
    }

    /// Defines variable at top scope
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.envs
            .last_mut()
            .expect("There should always be globals in the EnvironmentStack")
            .define(name, value);
    }

    /// add new empty env to the stack
    pub fn push_empty(&mut self) {
        self.envs.push(Environment::new())
    }
    pub fn push(&mut self, env: Environment) {
        self.envs.push(env)
    }
    pub fn pop(&mut self) -> Option<Environment> {
        self.envs.pop()
    }

    fn ancestor_mut(&mut self, distance: usize) -> Option<&mut Environment> {
        self.envs.iter_mut().rev().nth(distance)
    }

    fn ancestor(&self, distance: usize) -> Option<&Environment> {
        self.envs.iter().rev().nth(distance)
    }

    pub fn get_at(&self, distance: usize, name: &str) -> Option<&LoxValue> {
        match self.ancestor(distance) {
            Some(env) => env.values.get(name),
            None => None,
        }
    }

    pub fn assign_at(&mut self, distance: usize, name: &str, value: LoxValue) -> bool {
        // TODO this coalesces the error of assignment failing and the ancestor not being able to be found
        match self.ancestor_mut(distance) {
            Some(env) => env.assign(name, value),
            None => false,
        }
    }

    /// returns true if assign completed on any env, false if the variable was not found
    pub fn assign(&mut self, name: &str, value: LoxValue) -> bool {
        // Search from the top of the stack and try to assign in each env
        for env in self.envs.iter_mut().rev() {
            if env.contains_key(name) {
                return env.assign(name, value);
            }
        }
        false
    }
    /// returns first value found in any environment
    pub fn get(&self, name: &str) -> Option<&LoxValue> {
        for env in self.envs.iter().rev() {
            if let Some(v) = env.get(name) {
                return Some(v);
            }
        }
        None
    }
    pub fn get_copy(&self, name: &str) -> Option<LoxValue> {
        self.get(name).map(|v| v.clone())
    }

    /// Gets env at the closest scope
    pub fn get_top_env(&self) -> &Environment {
        self.envs
            .last()
            .expect("There should always be globals in the EnvironmentStack")
    }

    pub fn get_top_env_mut(&mut self) -> &mut Environment {
        self.envs
            .last_mut()
            .expect("There should always be globals in the EnvironmentStack")
    }

    pub fn get_global_env_mut(&mut self) -> &mut Environment {
        self.envs
            .first_mut()
            .expect("There should always be globals in the EnvironmentStack")
    }
    pub fn get_global_env(&self) -> &Environment {
        self.envs
            .first()
            .expect("There should always be globals in the EnvironmentStack")
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    // Allows redefinition of a variable in a single scope
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }
    pub fn get(&self, name: &str) -> Option<&LoxValue> {
        self.values.get(name)
    }
    pub fn get_copy(&self, name: &str) -> Option<LoxValue> {
        self.values.get(name).map(|v| v.clone())
    }
    pub fn contains_key(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    /// return true if assignment suceeded, false if not
    pub fn assign(&mut self, name: &str, value: LoxValue) -> bool {
        if let Some(v) = self.values.get_mut(name) {
            *v = value;
            return true;
        }
        return false;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;

    #[test]
    fn set_and_get() {
        let mut env = Environment::new();
        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);

        env.define(&token.lexeme, LoxValue::Bool(true));

        let value = env.get(&token.lexeme);
        assert!(value.is_some());

        let value = value.unwrap();
        assert_eq!(*value, LoxValue::Bool(true), "Value was {:?}", value);
    }

    #[test]
    fn get_from_enclosing() {
        let token = Token::new(TokenKind::Identifier, "foo".to_string(), 1);
        let mut globals = Environment::new();
        globals.define(&token.lexeme, LoxValue::Bool(true));

        let mut env_stack = EnvironmentStack::new(globals);

        let value = env_stack.get(&token.lexeme);
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(*value, LoxValue::Bool(true), "Value was {:?}", value);

        // Create an env that is enclosed by the global env
        env_stack.push_empty();

        let value = env_stack.get(&token.lexeme);
        assert!(value.is_some());
        let value = value.unwrap();
        assert_eq!(*value, LoxValue::Bool(true), "Value was {:?}", value);
    }
}
