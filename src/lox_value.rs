use crate::{
    environment::Environment,
    interpreter::{self, EvalError, Interpreter},
    stmt,
    token::Token,
};
use std::fmt;
use std::rc::Rc;

/// A callable lox object
pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[LoxValue],
    ) -> Result<LoxValue, interpreter::EvalError>;
}

pub struct LoxFunction {
    // Equivalent to stmt::Stmt::Function
    name: Token,
    params: Vec<Token>,
    body: Vec<stmt::Stmt>,
}

impl LoxFunction {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<stmt::Stmt>) -> LoxFunction {
        LoxFunction { name, params, body }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[LoxValue],
    ) -> Result<LoxValue, interpreter::EvalError> {
        let env = Environment::new_with_enclosing(interpreter.get_globals());
        // copy the arguments into the current environment
        for i in 0..self.params.len() {
            let lexeme = &self.params[i].lexeme;
            let arg = arguments[i].clone();
            env.borrow_mut().define(lexeme, arg);
        }

        // Super hacky, but return values are bubbling up the callstack as errors
        if let Err(e) = interpreter.execute_block(&self.body, env) {
            match e {
                EvalError::Return(value) => Ok(value),
                // Just forward the rest up
                _ => Err(e),
            }
        } else {
            Ok(LoxValue::Nil)
        }
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", &self.name.lexeme)
    }
}

/// This somewhat duplicated tokens::LiteralKind but eschews the identifier and None type
#[derive(Clone)]
pub enum LoxValue {
    // Possible value types in lox
    String(String),
    Number(f64),
    Bool(bool),
    // Eugh, so this needs to be sized and copyable
    // So i have to use a smart pointer to get indirection but i also need it to be copyable
    // hopefully i dont need to mutate any of the callables
    // If i do, i could just copy a new one i guess
    Callable(Rc<dyn LoxCallable>),
    Nil,
}

impl LoxValue {
    /// Follows ruby rules where everything is truthy except false and nil
    pub fn is_truthy(&self) -> bool {
        match &self {
            LoxValue::String(_) => true,
            LoxValue::Number(_) => true,
            LoxValue::Bool(b) => b.to_owned(),
            LoxValue::Nil => false,
            LoxValue::Callable(_) => true, // unsure if this is right, maybe it should evaluate the expression?
        }
    }
    pub fn as_numbers(
        line_num: usize,
        lhs: LoxValue,
        rhs: LoxValue,
    ) -> Result<(f64, f64), EvalError> {
        match (&lhs, &rhs) {
            (LoxValue::Number(l), LoxValue::Number(r)) => Ok((*l, *r)),
            (LoxValue::Number(_), _) => Err(EvalError::InvalidType(
                line_num,
                format!("Could not convert rhs operand '{}' to a number", rhs),
            )),
            (_, LoxValue::Number(_)) => Err(EvalError::InvalidType(
                line_num,
                format!("Could not convert lhs operand '{}' to a number", lhs),
            )),
            (_, _) => Err(EvalError::InvalidType(
                line_num,
                format!(
                    "Could not convert lhs '{}' nor rhs '{}' operands to a number",
                    lhs, rhs
                ),
            )),
        }
    }
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LoxValue::String(s) => write!(f, "{}", s),
            LoxValue::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    // Lop off the .0 if it's there
                    write!(f, "{:.1}", s)
                } else {
                    write!(f, "{}", s)
                }
            }
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Callable(_) => write!(f, "callable"),
        }
    }
}

impl fmt::Debug for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            LoxValue::String(s) => write!(f, "{:?}", s),
            LoxValue::Number(n) => write!(f, "{:?}", n),
            LoxValue::Bool(b) => write!(f, "{:?}", b),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Callable(_) => write!(f, "callable"),
        }
    }
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            // Just say that boxes are note comparable for now
            (Self::Callable(_), Self::Callable(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
