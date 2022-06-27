use crate::error::LoxError;
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::{LiteralKind, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
    non_fatal_errors: Vec<LoxError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            cur_idx: 0,
            non_fatal_errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<LoxError>> {
        let mut statements = Vec::new();
        while !self.is_at_end().map_err(|e| vec![e])? {
            statements.push(self.declaration().map_err(|e| vec![e])?);
        }

        match self.non_fatal_errors.is_empty() {
            true => Ok(statements),
            false => Err(self.non_fatal_errors.clone()),
        }
    }

    /// Discard tokens until we reach a new synchronization point
    /// Useful for using after a syntax error occurs
    fn synchronize(&mut self) -> Result<(), LoxError> {
        self.advance()?;
        while !self.is_at_end()? {
            if self.previous()?.kind == TokenKind::SemiColon {
                return Ok(());
            }
            match self.peek()?.kind {
                // Any of these are synchronization points
                TokenKind::Class
                | TokenKind::For
                | TokenKind::Fun
                | TokenKind::If
                | TokenKind::Print
                | TokenKind::Return
                | TokenKind::Var
                | TokenKind::While => return Ok(()),
                // Not at a synchronization point, keep advancign
                _ => (),
            }
            self.advance()?;
        }
        Ok(())
    }

    fn consume(&mut self, token_type: &TokenKind, msg: &str) -> Result<Token, LoxError> {
        if self.check(token_type)? {
            return self.advance();
        }
        Err(LoxError::new_parser_err(self.peek()?, msg))
    }
    fn previous(&mut self) -> Result<Token, LoxError> {
        self.tokens
            .get(self.cur_idx - 1)
            .ok_or_else(|| {
                LoxError::Scanner(format!(
                "cur_idx={} it out of range of tokens.len()={}, you may be missing an EOF token",
                self.cur_idx,
                self.tokens.len()
            ))
            })
            .cloned()
    }

    fn peek(&self) -> Result<Token, LoxError> {
        self.tokens
            .get(self.cur_idx)
            .ok_or_else(||LoxError::Scanner(format!(
                "cur_idx={} is out of range of tokensreturn .len={}, you may be missing an EOF token",
                self.cur_idx,
                self.tokens.len()
            )))
            .cloned()
    }

    /// Checks that the current token being parsed is of a given type
    fn check(&self, token_type: &TokenKind) -> Result<bool, LoxError> {
        if self.is_at_end()? {
            return Ok(false);
        }
        Ok(&self.peek()?.kind == token_type)
    }

    fn is_at_end(&self) -> Result<bool, LoxError> {
        Ok(self.peek()?.kind == TokenKind::Eof)
    }

    /// Consumes a token, returns the previous even if we're at the end
    fn advance(&mut self) -> Result<Token, LoxError> {
        if !self.is_at_end()? {
            self.cur_idx += 1;
        }
        self.previous()
    }

    /// Return true if the token matches any of the given types
    fn token_matches(&mut self, token_types: &[&TokenKind]) -> Result<bool, LoxError> {
        for token_type in token_types {
            if self.check(token_type)? {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    // ------------------------------------------------------------------
    // Grammar rules
    // ------------------------------------------------------------------

    /// Grammar rule: program -> declaration* EOF;

    /// Grammar rule: varDecl -> "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> Result<Stmt, LoxError> {
        let name = self.consume(&TokenKind::Identifier, "Expect variable name.")?;

        // The initializing expression is entirely optional
        let initializer = if self.token_matches(&[&TokenKind::Equal])? {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            &TokenKind::SemiColon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::Var(name, initializer))
    }

    /// This is a synchronizatin point, it will synchronize on parsing errors
    /// Grammar rule: declaration -> varDecl | statement ;
    fn declaration(&mut self) -> Result<Stmt, LoxError> {
        let mut parsing_attempt = || -> Result<Stmt, LoxError> {
            if self.token_matches(&[&TokenKind::Var])? {
                return self.var_decl();
            }
            return self.statement();
        };
        match parsing_attempt() {
            Ok(v) => Ok(v),
            Err(e) => {
                // Syncrhonize on any errors then return an error
                self.synchronize()?;
                return Err(e);
            }
        }
    }

    /// Grammar rule: statement -> exprStmt | printStmt | block ;
    fn statement(&mut self) -> Result<Stmt, LoxError> {
        if self.token_matches(&[&TokenKind::Print])? {
            return self.print_statement();
        }
        if self.token_matches(&[&TokenKind::LeftBrace])? {
            return self.block();
        }

        // if it's not a print statement, assume it's an expression statement
        self.expression_statement()
    }

    /// Grammar rule: block -> "{" declaration* "}" ;
    fn block(&mut self) -> Result<Stmt, LoxError> {
        let mut statements = Vec::new();
        while !self.check(&TokenKind::RightBrace)? && self.is_at_end()? {
            statements.push(self.declaration()?);
        }
        self.consume(&TokenKind::RightBrace, "Expect '}' after block.")?;
        Ok(Stmt::Block(statements))
    }

    /// Grammar rule: exprStmt -> expression ";" ;
    fn expression_statement(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.consume(
            &TokenKind::SemiColon,
            &format!("expected ';' after expression {:?}", expr),
        )?;
        Ok(Stmt::Expression(expr))
    }

    /// Grammar rule: printStmt -> "print" expression ";" ;
    fn print_statement(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        self.consume(
            &TokenKind::SemiColon,
            &format!("expected ';' after expression {:?}", expr),
        )?;
        Ok(Stmt::Print(expr))
    }

    /// Grammar rule: expression -> equality ;
    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assignment()
    }

    /// Grammar rule: IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<Expr, LoxError> {
        let expr = self.equality()?;
        if self.token_matches(&[&TokenKind::Equal])? {
            let equals = self.previous()?;
            let value = self.assignment()?;

            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign(name, Box::from(value)));
            }

            // Chapter 8.4.1 says to just report this error instead of panicking and synchronizing
            let err_msg = format!("On line {}, invalid assignment target.", equals.line);
            eprintln!("{}", err_msg);
            self.non_fatal_errors.push(LoxError::Parser(err_msg));
        }
        Ok(expr)
    }

    /// Grammar rule: primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenKind::False])? {
            return Ok(Expr::Literal(LiteralKind::Bool(false)));
        }
        if self.token_matches(&[&TokenKind::True])? {
            return Ok(Expr::Literal(LiteralKind::Bool(true)));
        }
        if self.token_matches(&[&TokenKind::Nil])? {
            return Ok(Expr::Literal(LiteralKind::Nil));
        }
        if self.token_matches(&[&TokenKind::Number, &TokenKind::String])? {
            return Ok(Expr::Literal(self.previous()?.literal));
        }
        if self.token_matches(&[&TokenKind::Identifier])? {
            return Ok(Expr::Variable(self.previous()?));
        }
        if self.token_matches(&[&TokenKind::LeftParen])? {
            let expr = self.expression()?;
            // TODO just throw in msg here
            self.consume(&TokenKind::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::from(expr)));
        }
        Err(LoxError::new_parser_err(
            self.peek()?,
            "Expected expression but did not find one.",
        ))
    }

    /// Grammar rule: unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenKind::Bang, &TokenKind::Minus])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::from(right)));
        }
        self.primary()
    }

    /// Grammar rule: factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;

        while self.token_matches(&[&TokenKind::Slash, &TokenKind::Star])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: term -> factor ( ( "-" | "+" ) factor)* ;
    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;

        while self.token_matches(&[&TokenKind::Minus, &TokenKind::Plus])? {
            let operator = self.previous()?;
            let right = self.factor()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: comparison -> term ( ( ">" | ">=" | "<" | "<=") term)* ;
    fn comaprison(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term()?;

        while self.token_matches(&[
            &TokenKind::Greater,
            &TokenKind::GreaterEqual,
            &TokenKind::Less,
            &TokenKind::LessEqual,
        ])? {
            let operator = self.previous()?;
            let right = self.term()?;
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }

    /// Grammar rule: equality -> comparison ( ("!=" | "==") comparison )* ;
    /// Matches an equality or anything of higher precedence (ex: comparison)
    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.comaprison()?;
        // This pattern could be shared somehow
        while self.token_matches(&[&TokenKind::BangEqual, &TokenKind::EqualEqual])? {
            let operator = self.previous()?;
            let right = self.comaprison()?;
            // TODO Don't allocate in a hot loop. This is a bit too 1:1 with the java impl for my comfort
            // This creates an expression including itself if there are multiple equality comparisons
            expr = Expr::Binary(Box::from(expr), operator, Box::from(right));
        }
        Ok(expr)
    }
    // ------------------------------------------------------------------
    // end grammar rules
    // ------------------------------------------------------------------
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::expr::Expr;
    use crate::scanner::Scanner;
    use crate::stmt;
    use crate::token::*;

    #[test]
    fn equality_test() {
        let line = 1;
        let tokens = vec![
            Token::new_literal(LiteralKind::Number(5.0), line),
            Token::new(TokenKind::EqualEqual, "==".to_string(), line),
            Token::new_literal(LiteralKind::Number(1.0), line),
            Token::new(TokenKind::SemiColon, ";".to_string(), 1),
            Token::new(TokenKind::Eof, "EOF".to_string(), line),
        ];
        let mut parser = Parser::new(tokens);
        match parser.equality() {
            Ok(e) => match e {
                Expr::Binary(lhs, op, rhs) => {
                    assert_eq!(*lhs, Expr::Literal(LiteralKind::Number(5.0)));
                    assert_eq!(op.kind, TokenKind::EqualEqual);
                    assert_eq!(*rhs, Expr::Literal(LiteralKind::Number(1.0)));
                }
                e => assert!(
                    false,
                    "Expected equality to return Expr::Binary and not {:?}",
                    e
                ),
            },
            Err(e) => assert!(false, "{:?}", e),
        }
    }

    #[test]
    fn parse_tokens_from_scanner() {
        let input = "1 * 2 + (3 - 5);".to_string();
        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);
        let res = parser.parse();
        assert!(
            res.is_ok(),
            "{}",
            res.unwrap_err().first().unwrap().to_string()
        );
    }

    #[test]
    fn parse_invalid_tokens_from_scanner() {
        let input = "*( 2 + (3 - 5);".to_string();
        let mut scan = Scanner::new(input);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());

        let tokens = scan.copy_tokens();

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);
        let res = parser.parse();
        assert!(res.is_err());
    }

    #[test]
    fn parse_tokens() {
        let tokens = vec![
            Token::new(TokenKind::Minus, "-".to_string(), 1),
            Token::new_literal(LiteralKind::Number(123.0), 1),
            Token::new(TokenKind::Star, "*".to_string(), 1),
            Token::new_literal(LiteralKind::Number(45.67), 1),
            Token::new(TokenKind::SemiColon, ";".to_string(), 1),
            Token::new(TokenKind::Eof, "EOF".to_string(), 1),
        ];

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);

        let res = parser.parse();
        assert!(
            res.is_ok(),
            "{}",
            res.unwrap_err().first().unwrap().to_string()
        );
        let statements = res.unwrap();
        assert_eq!(statements.len(), 1);
        match &statements[0] {
            stmt::Stmt::Expression(expr) => match expr {
                Expr::Binary(_, _, _) => assert!(true),
                _ => assert!(false, "Expected a binary expression"),
            },
            _ => assert!(false, "Expected an expression statement"),
        }
    }
}
