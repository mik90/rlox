use crate::{
    environment::{Environment, EnvironmentStack},
    error::ErrorMessage,
    expr::{self, Expr},
    lox_value::{LoxCallable, LoxFunction, LoxValue},
    resolver::Resolver,
    stmt,
    token::{LiteralKind, Token, TokenKind},
};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::rc::Rc;
use std::time;

#[derive(Debug)]
pub enum EvalError {
    UnreachableError(ErrorMessage),
    // Line number, error msg
    InvalidType(usize, ErrorMessage),
    UndefinedVariable(Token),
    /// Hacky, but this is analgous to how jlox uses try/catch for control flow in return values
    Return(LoxValue),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalError::UnreachableError(e) => write!(f, "UnreachableError: {}", e),
            EvalError::InvalidType(l, e) => write!(f, "InvalidType: On line {}, message: {}", l, e),
            EvalError::UndefinedVariable(token) => write!(f, "Undefined variable: {}", token),
            EvalError::Return(value) => write!(f, "Return value: {:?}", value),
        }
    }
}

impl error::Error for EvalError {}

pub struct Interpreter {
    /// ew why does this have to be public? For exposure to LoxCallable but why cant this be an arg?
    pub envs: EnvironmentStack,
    // Expression to 'depth' (scopes between current scope (top of envionment stack) and the one where a variable is defined)
    // TODO May need to have a way to uniquely identify expressions
    locals: HashMap<expr::Expr, usize>,
}

/// Returns the time since the epoch in seconds as a double
struct NativeClock {}
impl LoxCallable for NativeClock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &mut Interpreter, _: &[LoxValue]) -> Result<LoxValue, EvalError> {
        match time::SystemTime::now().duration_since(time::SystemTime::UNIX_EPOCH) {
            Ok(t) => Ok(LoxValue::Number(t.as_secs_f64())),
            Err(t) => Err(EvalError::UnreachableError(format!(
                "System time {} is before unix epoch!",
                t
            ))),
        }
    }
}
impl fmt::Display for NativeClock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl Interpreter {
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        expr.accept(self)
    }

    pub fn new() -> Interpreter {
        let mut globals = Environment::new();
        globals.define("clock", LoxValue::Callable(Rc::new(NativeClock {})));
        Interpreter {
            envs: EnvironmentStack::new(globals),
            locals: HashMap::new(),
        }
    }

    #[cfg(test)]
    pub fn get_environment(&self) -> &Environment {
        self.envs.get_top_env()
    }

    #[cfg(test)]
    pub fn get_environment_mut(&mut self) -> &mut Environment {
        self.envs.get_top_env_mut()
    }

    pub fn get_globals(&self) -> &Environment {
        self.envs.get_global_env()
    }

    pub fn resolve(&mut self, expr: &Expr, scope_distance: usize) -> Result<(), EvalError> {
        self.locals.insert(expr.clone(), scope_distance);
        Ok(())
    }

    fn execute(&mut self, stmt: &stmt::Stmt) -> Result<(), EvalError> {
        stmt.accept(self)
    }

    fn look_up_variable(&self, name: &Token, expr: &expr::Expr) -> Option<&LoxValue> {
        if let Some(distance) = self.locals.get(expr) {
            self.envs.get_at(*distance, &name.lexeme)
        } else {
            self.envs.get_global_env().get(&name.lexeme)
        }
    }

    // Execute a block but swap the environment before execution
    pub fn execute_block_with_env(
        &mut self,
        statements: &[stmt::Stmt],
        env_stack: &mut EnvironmentStack,
    ) -> Result<(), EvalError> {
        std::mem::swap(env_stack, &mut self.envs);
        let res = self.execute_block(statements);
        std::mem::swap(env_stack, &mut self.envs);
        res
    }
    /// Helper for the Stmt visitor
    pub fn execute_block(&mut self, statements: &[stmt::Stmt]) -> Result<(), EvalError> {
        let mut execute_statements = || -> Result<(), EvalError> {
            // push a new env onto our current stack since we're in a new blcok
            self.envs.push_empty();
            for stmt in statements {
                self.execute(stmt)?;
            }
            Ok(())
        };

        let res = execute_statements();
        // Reset the env in case any stmt couldnt be executed
        self.envs.pop();

        res
    }

    /// Interpret an expression, return true on success and false on error
    pub fn interpret(&mut self, statements: Vec<stmt::Stmt>) -> bool {
        for stmt in statements {
            if let Err(e) = self.execute(&stmt) {
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

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let left = self.evaluate(lhs)?;

        if op.kind == TokenKind::Or {
            if left.is_truthy() {
                return Ok(left);
            }
        } else {
            // Not an or, if lhs is falsey then just return that
            if !left.is_truthy() {
                return Ok(left);
            }
        }

        self.evaluate(rhs)
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
        self.envs
            .get_copy(&name.lexeme)
            .ok_or_else(|| EvalError::UndefinedVariable(name.clone()))
    }

    fn visit_assign(&mut self, name: &Token, value_expr: &Expr) -> Result<LoxValue, EvalError> {
        let value = self.evaluate(value_expr)?;
        if let Some(distance) = self.locals.get(value_expr) {
            self.envs.assign_at(*distance, &name.lexeme, value.clone());
        } else {
            self.envs
                .get_global_env_mut()
                .assign(&name.lexeme, value.clone());
        }
        Ok(value)
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<LoxValue, EvalError> {
        let mut evaluated_args = Vec::new();
        for arg in arguments {
            evaluated_args.push(self.evaluate(arg)?);
        }

        let function = match self.evaluate(callee)? {
            LoxValue::Callable(function) => function,
            non_callable_type => {
                return Err(EvalError::InvalidType(
                    paren.line,
                    format!(
                        "Can only call functions and classes. Cannot call type {:?}",
                        non_callable_type
                    ),
                ));
            }
        };

        if arguments.len() != function.arity() {
            return Err(EvalError::InvalidType(
                paren.line,
                format!(
                    "Expected {} arguments but got {}.",
                    function.arity(),
                    arguments.len()
                ),
            ));
        }
        function.call(self, &evaluated_args)
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
        println!("{}", value);
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

        self.envs
            .define(&name.lexeme, value.unwrap_or(LoxValue::Nil));
        Ok(())
    }

    fn visit_block_stmt(&mut self, statements: &[stmt::Stmt]) -> Result<(), EvalError> {
        self.execute_block(statements)?;
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &stmt::Stmt,
        else_branch: &Option<Box<stmt::Stmt>>,
    ) -> Result<(), EvalError> {
        if self.evaluate(condition)?.is_truthy() {
            self.execute(then_branch)
        } else if let Some(else_branch) = else_branch {
            self.execute(else_branch)
        } else {
            // The 'if' was falsy and there was no 'else'
            Ok(())
        }
    }

    fn visit_while_stmt(&mut self, condition: &Expr, body: &stmt::Stmt) -> Result<(), EvalError> {
        while self.evaluate(condition)?.is_truthy() {
            self.execute(body)?;
        }
        Ok(())
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[stmt::Stmt],
    ) -> Result<(), EvalError> {
        // Right here is where im kinda breaking recursion.
        // Ideally the closure would be able to have the function itself included but
        // creating a function requires copying the current environment stack and they
        // do not share a common ancestry after a copy
        let function = LoxFunction::new(
            name.clone(),
            params.to_vec(),
            body.to_vec(),
            self.envs.clone(),
        );
        self.envs
            .define(&name.lexeme, LoxValue::Callable(Rc::new(function)));
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        _keyword: &Token,
        value: &Option<Expr>,
    ) -> Result<(), EvalError> {
        let value: LoxValue = value
            .as_ref()
            .map_or_else(|| Ok(LoxValue::Nil), |expr| self.evaluate(&expr))?;
        // Hacky, but we don't have exceptions here and I am NOT using a panic handler for this
        Err(EvalError::Return(value.clone()))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;
    use crate::{expr::Expr, stmt::Stmt};

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

        let res = interpreter.execute(&stmt);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());

        let stmt = print_variable("foo");
        let res = interpreter.execute(&stmt);
        assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
    }

    #[test]
    fn eval_var_update() {
        let mut interpreter = Interpreter::new();

        let stmt = declare_and_init_number("foo", 5.0);

        {
            let res = interpreter.execute(&stmt);
            assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
            let env = interpreter.get_environment();
            let value = env.get_copy("foo");
            assert!(value.is_some());
            assert_eq!(value.unwrap(), LoxValue::Number(5.0));
        }

        {
            let stmt = assign_to_var("foo", 10.0);

            let res = interpreter.execute(&stmt);
            assert!(res.is_ok(), "evaluate() failed with {:?}", res.err());
            let env = interpreter.get_environment();
            let value = env.get_copy("foo");
            assert!(value.is_some());
            assert_eq!(value.unwrap(), LoxValue::Number(10.0));
        }
    }

    #[test]
    fn native_call() {
        let mut interpreter = Interpreter::new();

        let env = interpreter.get_environment();
        let clock_func = env.get_copy("clock");
        assert!(clock_func.is_some());
        let clock_func = clock_func.unwrap();
        if let LoxValue::Callable(callable) = clock_func {
            assert_eq!(callable.arity(), 0);
            let lox_value = callable.call(&mut interpreter, &[]);
            assert!(
                lox_value.is_ok(),
                "Expected Ok but was {}",
                lox_value.is_err()
            );
            if let LoxValue::Number(n) = lox_value.unwrap() {
                // Time should be past the unix epoch
                assert!(n > 0.0);
            }
        } else {
            assert!(
                false,
                "Expected clock_func to be a callable but was {:?}",
                clock_func
            );
        }
    }
}
