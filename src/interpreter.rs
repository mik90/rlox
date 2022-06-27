use crate::environment::Environment;
use crate::error::ErrorMessage;
use crate::expr;
use crate::expr::Expr;
use crate::stmt;
use crate::token::TokenKind;
use crate::token::{LiteralKind, Token};
use std::cell::RefCell;
use std::error;
use std::fmt;
use std::rc::Rc;

/// This somewhat duplicated tokens::LiteralKind but eschews the identifier and None type
#[derive(Debug, PartialEq, Clone)]
pub enum LoxValue {
    // Possible value types in lox
    String(String),
    Number(f64),
    Bool(bool),
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
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    UnreachableError(ErrorMessage),
    // Line number, error msg
    InvalidType(usize, ErrorMessage),
    UndefinedVariable(Token),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalError::UnreachableError(e) => write!(f, "UnreachableError: {}", e),
            EvalError::InvalidType(l, e) => write!(f, "InvalidType: On line {}, message: {}", l, e),
            EvalError::UndefinedVariable(token) => write!(f, "Undefined variable: {}", token),
        }
    }
}

impl error::Error for EvalError {}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        expr.accept(self)
    }

    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new(),
        }
    }

    fn execute(&mut self, stmt: stmt::Stmt) -> Result<(), EvalError> {
        stmt.accept(self)
    }
    /// Interpret an expression, return true on success and false on error
    pub fn interpret(&mut self, statements: Vec<stmt::Stmt>) -> bool {
        for stmt in statements {
            if let Err(e) = self.execute(stmt) {
                eprintln!("Error during interpret(): {}", e);
                return false;
            }
        }
        true
    }
}

impl expr::Visitor<Result<LoxValue, EvalError>> for Interpreter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let (left, right) = (self.evaluate(lhs)?, self.evaluate(rhs)?);
        let line = op.line;

        match op.kind {
            // Arithmetic operations on numbers
            TokenKind::Minus => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l - r))
            },
            TokenKind::Slash => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l / r))
            },
            TokenKind::Star => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Number(l * r))
            },

            // Comparisons
            TokenKind::Greater => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l > r))
            },
            TokenKind::GreaterEqual => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l >= r))
            },
            TokenKind::Less => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l < r))
            },
            TokenKind::LessEqual => {
                let (l, r) = LoxValue::as_numbers(line, left, right)?;
                 Ok(LoxValue::Bool(l <= r))
            },

            // Aithmetic and concat operations
            TokenKind::Plus => match (left, right) {
                // String concatenation
                (LoxValue::String(l), LoxValue::String(r)) => {
                    Ok(LoxValue::String(format!("{l}{r}")))
                }
                (LoxValue::Number(l), LoxValue::Number(r)) => Ok(LoxValue::Number(l + r)),
                (l, r) => Err(EvalError::InvalidType(op.line, format!(
                    "Cannot use operator '+' with left hand side operand '{:?}' and right side operand '{:?}'",
                     l, r
                ))),
            },

            // Unknown
            _ => Err(EvalError::InvalidType(op.line,format!(
                "Unexpected operator '{:?}' for binary expression",
                op,
            ))),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> Result<LoxValue, EvalError> {
        match value {
            LiteralKind::String(s) => Ok(LoxValue::String(s.clone())),
            LiteralKind::Number(n) => Ok(LoxValue::Number(*n)),
            LiteralKind::Bool(b) => Ok(LoxValue::Bool(*b)),
            LiteralKind::Nil => Ok(LoxValue::Nil),
            // TODO unsure how to handle an Identifier here since there's no LoxValue for it
            _ => Err(EvalError::UnreachableError(format!(
                "Expected literal but found {value:?}"
            ))),
        }
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxValue, EvalError> {
        // evaluate the rhs of the unary
        let rhs_value = self.evaluate(right)?;

        // apply the operator to the rhs
        match op.kind {
            TokenKind::Minus => {
                if let LoxValue::Number(n) = rhs_value {
                    Ok(LoxValue::Number(-n))
                } else {
                    Err(EvalError::InvalidType(
                        op.line,
                        format!("Cannot use unary '-' with {rhs_value:?}"),
                    ))
                }
            }
            TokenKind::Bang => Ok(LoxValue::Bool(!rhs_value.is_truthy())),
            _ => Err(EvalError::InvalidType(
                op.line,
                format!(
                    "Cannot use {:?} as the operator in a unary expression",
                    op.kind
                ),
            )),
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<LoxValue, EvalError> {
        self.environment
            .borrow()
            .get(&name.lexeme)
            .ok_or(EvalError::UndefinedVariable(name.clone()))
    }

    fn visit_assign(&mut self, name: &Token, value: &Expr) -> Result<LoxValue, EvalError> {
        let value = self.evaluate(value)?;

        match self
            .environment
            .borrow_mut()
            .assign(&name.lexeme, value.clone())
        {
            true => Ok(value),
            false => Err(EvalError::UndefinedVariable(name.clone())),
        }
    }
}

impl stmt::Visitor<EvalError> for Interpreter {
    fn visit_expression_stmt(&mut self, expr: &Expr) -> Result<(), EvalError> {
        match self.evaluate(expr) {
            // This isn't an error in the sense that a statement that is
            // > 3 + 5
            // isn't an error
            Ok(_) => Ok(()), // discards the value from the expression statemetn
            Err(e) => Err(e),
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), EvalError> {
        let value = self.evaluate(expr)?;
        println!("{}", value.to_string());
        Ok(())
    }

    fn visit_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<Expr>,
    ) -> Result<(), EvalError> {
        let value = if let Some(initializing_expr) = initializer {
            Some(self.evaluate(initializing_expr)?)
        } else {
            None
        };

        self.environment
            .borrow_mut()
            .define(&name.lexeme, value.unwrap_or(LoxValue::Nil));
        Ok(())
    }

    fn visit_block(&mut self, statements: &Vec<stmt::Stmt>) -> Result<(), EvalError> {
        execute_block(
            statements,
            Environment::new_with_enclosing(self.environment.clone()),
        )?;
        Ok(())
    }
}

/// Helper for the Stmt visitor
fn execute_block(
    statements: &Vec<stmt::Stmt>,
    environment: Rc<RefCell<Environment>>,
) -> Result<(), EvalError> {
    todo!()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;
    use crate::{expr::Expr, stmt::Stmt};

    impl Interpreter {
        fn get_environment(&self) -> Rc<RefCell<Environment>> {
            self.environment.clone()
        }
    }

    fn number_expr(n: f64) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::Number(n)))
    }

    fn negated_number_expr(n: f64) -> Box<Expr> {
        Box::new(Expr::Unary(
            Token::new(TokenKind::Minus, "-".to_string(), 1),
            number_expr(n),
        ))
    }

    /// Declares a value of 'name' and inits it with 'value' in one statement
    fn declare_and_init_number(name: &str, value: f64) -> Stmt {
        let token = Token::new_literal(LiteralKind::Identifier(name.to_string()), 1);
        Stmt::Var(token, Some(Expr::Literal(LiteralKind::Number(value))))
    }

    fn assign_to_var(name: &str, value: f64) -> Stmt {
        let token = Token::new_literal(LiteralKind::Identifier(name.to_string()), 1);
        let expr = Expr::Assign(token, Box::new(Expr::Literal(LiteralKind::Number(value))));
        Stmt::Expression(expr)
    }

    fn print_variable(name: &str) -> Stmt {
        let var_to_print = Expr::Variable(Token::new_literal(
            LiteralKind::Identifier(name.to_string()),
            1,
        ));
        Stmt::Print(var_to_print)
    }

    fn string_expr(s: &str) -> Box<Expr> {
        Box::new(Expr::Literal(LiteralKind::String(s.to_owned())))
    }

    #[test]
    fn eval_addition() {
        let expr = Expr::Binary(
            number_expr(3.0),
            Token::new(TokenKind::Plus, "+".to_string(), 1),
            number_expr(3.0),
        );
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
        let res = res.unwrap();
        if let LoxValue::Number(n) = res {
            assert_eq!(n, 6.0)
        } else {
            assert!(false, "Expected LoxValue::Number but was {:?}", res)
        }
    }

    #[test]
    fn eval_muffin() {
        let expr = Expr::Binary(
            number_expr(3.0),
            Token::new(TokenKind::Slash, "/".to_string(), 1),
            string_expr("muffin"),
        );
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_err(), "Expected evaluate() to fail but it didn't");
    }

    #[test]
    fn eval_negated_number() {
        let expr = negated_number_expr(5.0);
        let mut interpreter = Interpreter::new();
        let res = interpreter.evaluate(&expr);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());

        let res = res.unwrap();
        if let LoxValue::Number(n) = res {
            assert_eq!(n, -5.0)
        } else {
            assert!(false, "Expected LoxValue::Number but was {:?}", res)
        }
    }
    #[test]
    fn eval_assignment() {
        let stmt = declare_and_init_number("foo", 5.0);

        let mut interpreter = Interpreter::new();

        let res = interpreter.execute(stmt);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());

        let stmt = print_variable("foo");
        let res = interpreter.execute(stmt);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
    }

    #[test]
    fn eval_var_update() {
        let mut interpreter = Interpreter::new();

        let stmt = declare_and_init_number("foo", 5.0);

        {
            let res = interpreter.execute(stmt);
            assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
            let env = interpreter.get_environment();
            let value = env.borrow().get("foo");
            assert!(value.is_some());
            assert_eq!(value.unwrap(), LoxValue::Number(5.0));
        }

        {
            let stmt = assign_to_var("foo", 10.0);

            let res = interpreter.execute(stmt);
            assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
            let env = interpreter.get_environment();
            let value = env.borrow().get("foo");
            assert!(value.is_some());
            assert_eq!(value.unwrap(), LoxValue::Number(10.0));
        }
    }
}
