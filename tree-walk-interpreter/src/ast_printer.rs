use crate::expr;
use crate::expr::Expr;
use crate::token::{LiteralKind, Token};

pub struct AstPrinter {}

impl AstPrinter {
    /// Only used for testing
    #[allow(dead_code)]
    pub fn print(expr: &Expr) -> String {
        let mut printer = AstPrinter {};
        expr.accept(&mut printer)
    }
}

fn parenthisize(visitor: &mut dyn expr::Visitor<String>, name: &str, exprs: &[&Expr]) -> String {
    let mut ast = format!("({}", name);

    for expr in exprs {
        ast.push(' ');
        let value = expr.accept(visitor);
        ast.push_str(&value);
    }
    ast.push(')');
    ast
}

impl expr::Visitor<String> for AstPrinter {
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        parenthisize(self, &op.lexeme, &[lhs, rhs])
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        parenthisize(self, "group", &[expr])
    }

    fn visit_literal(&mut self, value: &LiteralKind) -> String {
        value.to_string()
    }
    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        parenthisize(self, &op.lexeme, &[lhs, rhs])
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> String {
        parenthisize(self, &op.lexeme, &[right])
    }

    fn visit_variable(&mut self, name: &Token) -> String {
        name.lexeme.to_string()
    }

    /// May not be right
    fn visit_assign(&mut self, name: &Token, value: &Expr) -> String {
        parenthisize(self, &name.lexeme, &[value])
    }

    fn visit_call(&mut self, callee: &Expr, paren: &Token, arguments: &[Expr]) -> String {
        format!(
            "Not fully implemented, here's some debug logging\ncallee={:?}, paren={}, arguments={:?}",
            callee, paren.lexeme, arguments
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;

    #[test]
    fn print_ast() {
        let token_1 = Token::new(TokenKind::Minus, "-".to_string(), 1);
        let token_2 = Token::new(TokenKind::Star, "*".to_string(), 1);
        let literal_1 = Expr::Literal(LiteralKind::Number(123.0));
        let literal_2 = Expr::Literal(LiteralKind::Number(45.67));
        let expr = Expr::Binary(
            Box::new(Expr::Unary(token_1, Box::new(literal_1))),
            token_2,
            Box::new(Expr::Grouping(Box::new(literal_2))),
        );
        let text = AstPrinter::print(&expr);
        assert_eq!(text, "(* (- 123) (group 45.67))");
    }
}
