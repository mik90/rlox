use crate::herefmt;
use std::{fmt, sync::Arc, sync::Mutex};

/// Generic heap-allocated object container
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Obj {
    String(String),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Obj::String(s) => write!(f, "{}", s),
        }
    }
}

/// Primitives supported by the VM
#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(Arc<Mutex<Obj>>),
}

impl From<Obj> for Value {
    // TODO Figure out a way to track allocations since I'm not using the same method as the book
    fn from(o: Obj) -> Self {
        Value::Obj(Arc::new(Mutex::new(o)))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => write!(f, "{}", n),
            Self::Obj(o) => write!(f, "{}", o.lock().expect(&herefmt!("Cannot lock object"))),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Obj(l0), Self::Obj(r0)) => {
                let lhs: &Obj = &l0.lock().expect(&herefmt!("Cannot lock lhs"));
                let rhs: &Obj = &r0.lock().expect(&herefmt!("Cannot lock rhs"));
                lhs == rhs
            }
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => Some(l0.cmp(r0)),
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            (Self::Obj(l0), Self::Obj(r0)) => {
                let lhs: &Obj = &l0.lock().expect(&herefmt!("Cannot lock lhs"));
                let rhs: &Obj = &r0.lock().expect(&herefmt!("Cannot lock rhs"));
                lhs.partial_cmp(rhs)
            }
            _ => None,
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
