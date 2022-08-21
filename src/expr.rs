use crate::token::LiteralKind;
use crate::token::Token;

// Expr is boxed in order to allow this enum to be recursive
// Otherwise it wouldn't be able to figure out the size of Expr
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Expr {
    /// Binary   : Expr left, Token operator, Expr right
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Call     : Expr callee, Token paren, Vec<Expr> arguments
    Call(Box<Expr>, Token, Vec<Expr>),
    /// Grouping : Expr expression
    Grouping(Box<Expr>),
    /// Literal  : Object value
    Literal(LiteralKind),
    /// Logical  : Expr left, Token operator, Expr right
    Logical(Box<Expr>, Token, Box<Expr>),
    /// Unary    : Token operator, Expr right
    Unary(Token, Box<Expr>),
    /// Variable : Token name
    Variable(Token),
    /// Assign   : Token name, Expr Value
    Assign(Token, Box<Expr>),
}

pub trait Visitor<T> {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> T;
    fn visit_call(&mut self, callee: &Expr, paren: &Token, arguments: &[Expr]) -> T;
    fn visit_grouping(&mut self, expr: &Expr) -> T;
    fn visit_literal(&mut self, value: &LiteralKind) -> T;
    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> T;
    fn visit_unary(&mut self, op: &Token, right: &Expr) -> T;
    fn visit_variable(&mut self, name: &Token) -> T;
    fn visit_assign(&mut self, name: &Token, value: &Expr) -> T;
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Expr::Binary(lhs, op, rhs) => visitor.visit_binary(lhs, op, rhs),
            Expr::Call(callee, paren, arguments) => visitor.visit_call(callee, paren, arguments),
            Expr::Grouping(expr) => visitor.visit_grouping(expr),
            Expr::Literal(lit) => visitor.visit_literal(lit),
            Expr::Logical(lhs, op, rhs) => visitor.visit_logical(lhs, op, rhs),
            Expr::Unary(op, expr) => visitor.visit_unary(op, expr),
            Expr::Variable(name) => visitor.visit_variable(name),
            Expr::Assign(name, value) => visitor.visit_assign(name, value),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::hash_map::DefaultHasher;
    use std::collections::hash_map::HashMap;
    use std::hash::{Hash, Hasher};

    #[test]
    fn expr_hash() {
        let literal = LiteralKind::Identifier("i".to_string());

        let i_3 = Expr::Variable(Token::new_literal(literal.clone(), 3));

        let i_4 = Expr::Variable(Token::new_literal(literal.clone(), 4));

        let mut hasher = DefaultHasher::new();
        i_3.hash(&mut hasher);
        let hash_i_3 = hasher.finish();

        let mut hasher = DefaultHasher::new();
        i_4.hash(&mut hasher);
        let hash_i_4 = hasher.finish();
        assert_eq!(hash_i_3, hash_i_4);
    }

    #[test]
    fn expr_hash_map() {
        let mut map = HashMap::<Expr, usize>::new();

        let literal = LiteralKind::Identifier("i".to_string());
        let i_3 = Expr::Variable(Token::new_literal(literal.clone(), 3));
        let i_4 = Expr::Variable(Token::new_literal(literal.clone(), 4));

        assert!(map.insert(i_3, 0).is_none());

        assert!(
            map.get(&i_4).is_some(),
            "The same expr w/ diff line numbers should be interchangeable in the map"
        );
    }
}
