use crate::error::ErrorMessage;
use crate::expr::Expr;
use crate::expr::Visitor;
use crate::token::TokenKind;
use crate::token::{LiteralKind, Token};
use std::error;
use std::fmt;

pub struct Interpreter {}

/// This somewhat duplicated tokens::LiteralKind but eschews the identifier and None type
#[derive(Debug)]
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
}

impl TryInto<f64> for LoxValue {
    type Error = EvalError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            LoxValue::Number(n) => Ok(n),
            _ => Err(EvalError::InvalidType(format!(
                "Could not convert {:?} to a double",
                &self,
            ))),
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    UnreachableError(ErrorMessage),
    InvalidType(ErrorMessage),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            EvalError::UnreachableError(e) => write!(f, "UnreachableError - message: {}", e),
            EvalError::InvalidType(e) => write!(f, "InvalidType - message: {}", e),
        }
    }
}

impl error::Error for EvalError {}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        expr.accept(self)
    }
}

/// Possibly should switch this to an Option or Result
impl Visitor<Result<LoxValue, EvalError>> for Interpreter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Result<LoxValue, EvalError> {
        let (left, right) = (self.evaluate(lhs)?, self.evaluate(rhs)?);
        let (left, right): (f64, f64) = (left.try_into()?, right.try_into()?);

        match op.kind {
            TokenKind::Minus => Ok(LoxValue::Number(left - right)),
            TokenKind::Slash => Ok(LoxValue::Number(left / right)),
            TokenKind::Star => Ok(LoxValue::Number(left * right)),
            _ => Err(EvalError::UnreachableError(format!(
                "Unexpected operator '{:?}' for binary expression",
                op,
            ))),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<LoxValue, EvalError> {
        if let Expr::Grouping(g) = expr {
            self.evaluate(g)
        } else {
            Err(EvalError::UnreachableError(format!(
                "Expected grouping but found {expr:?}"
            )))
        }
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> Result<LoxValue, EvalError> {
        match value {
            LiteralKind::String(s) => Ok(LoxValue::String(s.clone())),
            LiteralKind::Number(n) => Ok(LoxValue::Number(n.clone())),
            LiteralKind::Bool(b) => Ok(LoxValue::Bool(b.clone())),
            LiteralKind::Nil => Ok(LoxValue::Nil),
            _ => Err(EvalError::UnreachableError(format!(
                "Expected literal but found {value:?}"
            ))),
        }
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> Result<LoxValue, EvalError> {
        // evaluate the rhs of the unary
        let rhs_value = if let Expr::Unary(_, right) = right {
            self.evaluate(right)
        } else {
            Err(EvalError::UnreachableError(format!(
                "Expected Unary but found {right:?}"
            )))
        }?;

        // apply the operator to the rhs
        match op.kind {
            TokenKind::Minus => match rhs_value {
                LoxValue::Number(n) => Ok(LoxValue::Number(-n)),
                // Cannot apply unary minus to anything other than a number
                _ => Err(EvalError::InvalidType(format!(
                    "Cannot use unary '-' with {rhs_value:?}"
                ))),
            },
            TokenKind::Bang => Ok(LoxValue::Bool(!rhs_value.is_truthy())),
            // No other operators are supported for the rhs
            _ => Err(EvalError::InvalidType(format!(
                "Cannot use {:?} as the operator in a unary expression",
                op.kind
            ))),
        }
    }
}
