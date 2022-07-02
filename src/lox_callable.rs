use crate::interpreter::{Interpreter, LoxValue};
/// A callable lox object
pub trait LoxCallable<T> {
    // Unsure if lox values are generic enough
    fn call(interpreter: &mut Interpreter, arguments: &Vec<LoxValue>) -> LoxValue;
}
