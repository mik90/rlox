use crate::expr::Expr;
use crate::token::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, cur_idx: 0 }
    }
    fn comaprison(&mut self) -> Expr {
        todo!()
    }
    /// Grammar rule: equality -> comparison ( ("!=" | "==") comparison )* ;
    fn equality(&mut self) -> Expr {
        let mut expr = self.comaprison();
        while self.token_matches(&[&TokenType::BangEqual, &TokenType::EqualEqual]) {
            let op = self.previous();
            let right = self.comaprison();
            // TODO Don't allocate in a hot loop. This is a bit too 1:1 with the java impl for my comfort
            // This creates an expression including itself if there are multiple equality comparisons
            expr = Expr::Binary(Box::from(expr), op, Box::from(right));
        }
        expr
    }
    fn previous(&mut self) -> Token {
        // TODO not very safe :(
        self.tokens[self.cur_idx - 1].clone()
    }

    fn peek(&self) -> Token {
        // TODO not very safe :(
        self.tokens[self.cur_idx].clone()
    }

    /// Checks that the current token being parsed is of a given type
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().kind == token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenType::Eof
    }

    /// Consumes a token, returns the previous even if we're at the end
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.cur_idx += 1;
        }
        self.previous()
    }

    /// Return true if the token matches any of the given types
    fn token_matches(&mut self, token_types: &[&TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn expression(&mut self) -> Expr {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Expr;
    use crate::LiteralType;
    use crate::Token;
    use crate::TokenType;
    #[test]
    fn equality_test() {
        let tokens = vec![
            Token::new(
                TokenType::Number,
                "5".to_string(),
                LiteralType::Number(5.0),
                1,
            ),
            Token::new(
                TokenType::EqualEqual,
                "==".to_string(),
                LiteralType::None,
                1,
            ),
            Token::new(
                TokenType::Number,
                "1".to_string(),
                LiteralType::Number(1.0),
                1,
            ),
        ];
        let mut parser = Parser::new(tokens);
        match parser.equality() {
            Expr::Binary(lhs, op, rhs) => {
                assert_eq!(*lhs, Expr::Literal(LiteralType::Number(5.0)));
                assert_eq!(op.kind, TokenType::EqualEqual);
                assert_eq!(*rhs, Expr::Literal(LiteralType::Number(1.0)));
            }
            e => assert!(
                false,
                "Expected equality to return Expr::Binary and not {:?}",
                e
            ),
        }
    }
}
