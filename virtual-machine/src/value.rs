use std::fmt;

/// Primitives supported by the VM
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Value {
    pub fn falsey(&self) -> bool {
        match &self {
            Self::Nil => true,
            Self::Bool(b) => !b,
            _ => true, // Any type other than bool or nil is true
        }
    }
}

// Pool of constants
pub type ValueArray = Vec<Value>;
