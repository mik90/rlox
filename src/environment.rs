use crate::{interpreter::LoxValue, token::Token};
use std::collections::HashMap;

/// This is where all the variables live
pub struct Environment {
    /// variable names to values
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    // Allows redefinition of a variable
    pub fn define(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Option<LoxValue> {
        self.values.get(&name.lexeme).map(|value| value.clone())
    }

    // quite ugly in that get returns a copy while get_mut returns a ref
    pub fn get_mut(&mut self, name: &Token) -> Option<&mut LoxValue> {
        self.values.get_mut(&name.lexeme)
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

        let value = env.get(&token);
        assert!(value.is_some());

        let value = value.unwrap();
        assert_eq!(value, LoxValue::Bool(true), "Value was {:?}", value);
    }
}
