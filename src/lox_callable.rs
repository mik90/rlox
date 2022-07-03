use crate::{expr::Expr, interpreter::Interpreter, lox_value::LoxValue};
/// A callable lox object
pub trait LoxCallable {
    fn arity(&self) -> usize;
    fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Expr>) -> LoxValue;
}
