use crate::error::LoxError;
use crate::expr::Expr;
use crate::stmt::Stmt;
use crate::token::{LiteralKind, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
    non_fatal_errors: Vec<LoxError>,
}

const MAX_FUNC_PARAMS: usize = 255;

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
    /// Consume all the arguments in the call
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, LoxError> {
        let mut arguments = Vec::new();
        if !self.check(&TokenKind::RightParen)? {
            loop {
                if arguments.len() >= 255 {
                    self.non_fatal_errors.push(LoxError::new_parser_err(
                        self.peek()?,
                        "Can't have more than 255 arguments",
                    ));
                }

                arguments.push(self.expression()?);

                if !self.token_matches(&[&TokenKind::Comma])? {
                    break;
                }
            }
        }

        let paren = self.consume(&TokenKind::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call(Box::new(callee), paren, arguments))
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

    /// Grammar rule: funDecl -> "fun" function ;
    fn fun_decl(&mut self) -> Result<Stmt, LoxError> {
        todo!()
    }

    /// Grammar rule: function -> IDENTIFIER "(" parameters? ")" block ;
    fn function(&mut self, kind: &str) -> Result<Stmt, LoxError> {
        let name = self.consume(&TokenKind::Identifier, &format!("Expect {kind} name."))?;

        self.consume(
            &TokenKind::LeftParen,
            &format!("Expect '(' after {kind} name"),
        )?;
        let mut parameters = Vec::new();
        // Consume parameters between the parens
        if !self.check(&TokenKind::RightParen)? {
            loop {
                if parameters.len() >= MAX_FUNC_PARAMS {
                    return Err(LoxError::new_parser_err(
                        self.peek()?,
                        &format!("Can't have more than {MAX_FUNC_PARAMS} parameters."),
                    ));
                }
                parameters.push(self.consume(&TokenKind::Identifier, "Expect parameter name.")?);
                if !self.token_matches(&[&TokenKind::Comma])? {
                    break;
                }
            }
        }
        self.consume(&TokenKind::RightParen, "Expect ')' after parameters")?;
        self.consume(
            &TokenKind::LeftBrace,
            &format!("Expect '{{' before {kind} body"),
        )?;
        let body = self.block()?;
        Ok(Stmt::Function(name, parameters, body))
    }

    /// Grammar rule: parameters -> IDENTIFIER ( "," IDENTIFIER )* ;
    fn parameters(&mut self) -> Result<Stmt, LoxError> {
        todo!()
    }

    /// This is a synchronizatin point, it will synchronize on parsing errors
    /// Grammar rule: declaration -> funDecl | varDecl | statement ;
    fn declaration(&mut self) -> Result<Stmt, LoxError> {
        let mut parsing_attempt = || -> Result<Stmt, LoxError> {
            if self.token_matches(&[&TokenKind::Fun])? {
                return self.function("function");
            }
            if self.token_matches(&[&TokenKind::Var])? {
                return self.var_decl();
            }
            self.statement()
        };
        match parsing_attempt() {
            Ok(v) => Ok(v),
            Err(e) => {
                // Syncrhonize on any errors then return an error
                self.synchronize()?;
                Err(e)
            }
        }
    }

    /// Grammar rule: statement -> exprStmt | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
    fn statement(&mut self) -> Result<Stmt, LoxError> {
        if self.token_matches(&[&TokenKind::For])? {
            return self.for_statement();
        }
        if self.token_matches(&[&TokenKind::If])? {
            return self.if_statement();
        }
        if self.token_matches(&[&TokenKind::Print])? {
            return self.print_statement();
        }
        if self.token_matches(&[&TokenKind::Return])? {
            return self.return_statement();
        }
        if self.token_matches(&[&TokenKind::While])? {
            return self.while_statement();
        }
        if self.token_matches(&[&TokenKind::LeftBrace])? {
            let statements = self.block()?;
            return Ok(Stmt::Block(statements));
        }

        // if it's not a print statement, assume it's an expression statement
        self.expression_statement()
    }

    /// Desugars a for loop into a while statement with an optional initializer
    /// Grammar rule: forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    fn for_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(&TokenKind::LeftParen, "Expect '(' after 'for'")?;

        let initializer = if self.token_matches(&[&TokenKind::SemiColon])? {
            // Initializer is omitted
            None
        } else if self.token_matches(&[&TokenKind::Var])? {
            Some(self.var_decl()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&TokenKind::SemiColon)? {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenKind::SemiColon, "Expect ';' after loop condition")?;

        let increment = if !self.check(&TokenKind::RightParen)? {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenKind::RightParen, "Expect ')' after 'for' clauses")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            // Execute increment at the end of the body for each iteration
            body = Stmt::Block(vec![body, Stmt::Expression(inc)]);
        }

        // If there's no condition, make it into an infinite loop
        let condition = match condition {
            Some(condition) => condition,
            None => Expr::Literal(LiteralKind::Bool(true)),
        };
        body = Stmt::While(condition, Box::new(body));

        // add the initializer at the front of the block
        // Runs once before the loop
        if let Some(init) = initializer {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    }

    /// Grammar rule: whileStmt -> "while" "(" expression ")" statement ;
    fn while_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(&TokenKind::LeftParen, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Stmt::While(condition, Box::new(body)))
    }

    /// Grammar rule: ifStmt -> "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_statement(&mut self) -> Result<Stmt, LoxError> {
        self.consume(&TokenKind::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenKind::RightParen, "Expect ')' after if consition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.token_matches(&[&TokenKind::Else])? {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    /// This rule will be reused for function bodies so we don't want to always return a Stmt::Block
    /// Grammar rule: block -> "{" declaration* "}" ;
    fn block(&mut self) -> Result<Vec<Stmt>, LoxError> {
        let mut statements = Vec::new();
        while !self.check(&TokenKind::RightBrace)? && !self.is_at_end()? {
            statements.push(self.declaration()?);
        }
        self.consume(&TokenKind::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
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

    /// Grammar rule: returnStmt -> "return" expression? ";" ;
    fn return_statement(&mut self) -> Result<Stmt, LoxError> {
        let keyword = self.previous()?;
        let value = if !self.check(&TokenKind::SemiColon)? {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenKind::SemiColon, "Expect ';' after return value.")?;
        Ok(Stmt::Return(keyword, value))
    }

    /// Grammar rule: expression -> assignment ;
    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assignment()
    }

    /// Grammar rule: assignment -> IDENTIFIER "=" assignment | equality | logic_or;
    fn assignment(&mut self) -> Result<Expr, LoxError> {
        let expr = self.or()?;

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

    /// Grammar rule: logic_or -> logic_and ( "or" logic_and )* ;
    fn or(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.and()?;

        while self.token_matches(&[&TokenKind::Or])? {
            let operator = self.previous()?;
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    /// Grammar rule: logic_and -> equality ( "and" equality )* ;
    fn and(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.equality()?;

        while self.token_matches(&[&TokenKind::And])? {
            let operator = self.previous()?;
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
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

    /// Grammar rule: unary -> ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> Result<Expr, LoxError> {
        if self.token_matches(&[&TokenKind::Bang, &TokenKind::Minus])? {
            let operator = self.previous()?;
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::from(right)));
        }
        self.call()
    }

    /// Grammar rule: call -> primary ( "(" arguments? ")" )* ;
    fn call(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.primary()?;
        // consume as many arguments as exist
        loop {
            if self.token_matches(&[&TokenKind::LeftParen])? {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Grammar rule: arguments -> expression ( "," expression )* ;
    fn arguments(&mut self) -> Result<Expr, LoxError> {
        todo!()
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
    fn parse_func_from_scanner() {
        let code = r#"
fun foo(a, b) {
    print a + b;
}
"#
        .to_string();

        let mut scan = Scanner::new(code);
        let res = scan.scan_tokens();
        assert!(res.is_ok(), "{}", res.unwrap_err().to_string());
        let tokens = scan.copy_tokens();

        // This is where the tokens are actually tested
        let mut parser = Parser::new(tokens);

        let res = parser.parse();
        assert!(
            res.is_ok(),
            "Could not parse input, got {:?}",
            res.unwrap_err()
        );
        match res {
            Ok(v) => {
                assert_eq!(v.is_empty(), false);
                let stmt = &v[0];
                if let Stmt::Function(_, _, _) = stmt {
                    assert!(true);
                } else {
                    assert!(false, "Expected Stmt::Function but got {:?}", stmt);
                }
            }
            Err(e) => assert!(false, "{:?}", e),
        }
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
