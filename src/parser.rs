use crate::{
    error::{parser_error, Error},
    expr::{Expr, ExprVisitor},
    stmt::{Stmt, StmtVisitor},
    token::{Object, Token, TokenType},
};
use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

pub trait Parse {
    fn parse(&mut self) -> Result<Vec<Stmt>, Error>;
    fn decl(&mut self) -> Result<Stmt, Error>;
    fn var_decl(&mut self) -> Result<Stmt, Error>;
    fn stmt(&mut self) -> Result<Stmt, Error>;
    fn print_stmt(&mut self) -> Result<Stmt, Error>;
    fn expr(&mut self) -> Result<Expr, Error>;
    fn assign(&mut self) -> Result<Expr, Error>;
    fn expr_stmt(&mut self) -> Result<Stmt, Error>;
    fn or(&mut self) -> Result<Expr, Error>;
    fn and(&mut self) -> Result<Expr, Error>;
    fn equality(&mut self) -> Result<Expr, Error>;
    fn comparison(&mut self) -> Result<Expr, Error>;
    fn term(&mut self) -> Result<Expr, Error>;
    fn factor(&mut self) -> Result<Expr, Error>;
    fn unary(&mut self) -> Result<Expr, Error>;
    fn primary(&mut self) -> Result<Expr, Error>;
}

impl Parse for Parser {
    fn parse(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];
        while !self.is_at_end() {
            stmts.push(self.decl()?);
        }

        Ok(stmts)
    }

    fn decl(&mut self) -> Result<Stmt, Error> {
        let stmt = if self.r#match(&[TokenType::Var]) {
            self.var_decl()
        } else {
            self.stmt()
        };

        match stmt {
            Err(e) => {
                self.synchronize();
                Err(e)
            }
            stmt => stmt,
        }
    }

    fn var_decl(&mut self) -> Result<Stmt, Error> {
        let name = self.consume(&TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.r#match(&[TokenType::Equal]) {
            Some(self.expr()?)
        } else {
            None
        };

        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var { name, initializer })
    }

    fn stmt(&mut self) -> Result<Stmt, Error> {
        if self.r#match(&[TokenType::Print]) {
            self.print_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr()?;
        self.consume(&TokenType::Semicolon, "expected ';' after value.")?;
        Ok(Stmt::Print { expr })
    }

    fn expr(&mut self) -> Result<Expr, Error> {
        self.assign()
    }

    fn expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr()?;
        self.consume(&TokenType::Semicolon, "expected ';' after expr.")?;
        Ok(Stmt::Expr { expr })
    }

    fn assign(&mut self) -> Result<Expr, Error> {
        let expr = self.or()?;

        if self.r#match(&[TokenType::Equal]) {
            let value = Box::new(self.assign()?);
            if let Expr::Var { name } = expr {
                return Ok(Expr::Assign { name, expr: value });
            }
            let equals = &self.previous();
            self.error(equals, "Invalid assignment target.");
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.and()?;

        while self.r#match(&[TokenType::Or]) {
            let op = self.previous();
            let right = Box::new(self.and()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.r#match(&[TokenType::And]) {
            let op = self.previous();
            let right = Box::new(self.equality()?);
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right,
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.r#match(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while self.r#match(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = self.previous();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.r#match(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.r#match(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.r#match(&[TokenType::Plus]) {
            self.previous();
            return self.unary();
        }

        if self.r#match(&[TokenType::Minus]) {
            let op = self.previous();
            return Ok(Expr::Unary {
                op,
                expr: Box::new(self.unary()?),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.peek();

        let expr = if self.r#match(&[TokenType::False]) {
            Expr::Literal {
                value: Object::Bool(false),
            }
        } else if self.r#match(&[TokenType::True]) {
            Expr::Literal {
                value: Object::Bool(true),
            }
        } else if self.r#match(&[TokenType::Nil]) {
            Expr::Literal { value: Object::Nil }
        } else if self.r#match(&[TokenType::String, TokenType::Number]) {
            Expr::Literal {
                value: token.literal.unwrap_or_default(),
            }
        } else if self.r#match(&[TokenType::Identifier]) {
            Expr::Var { name: token }
        } else if self.r#match(&[TokenType::LeftParen]) {
            let expr = self.expr()?;
            self.consume(&TokenType::RightParen, "Expected ')' after expr.")?;
            Expr::Grouping {
                expr: Box::new(expr),
            }
        } else {
            return Err(Error::Runtime {
                token,
                message: "Expected expr".to_string(),
            });
        };

        Ok(expr)
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ..Default::default()
        }
    }

    fn r#match(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().r#type == *token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().r#type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<Token, Error> {
        if self.check(token_type) {
            Ok(self.advance().clone())
        } else {
            Err(self.error(&self.peek(), message))
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().r#type == TokenType::Semicolon {
                return;
            }

            if let TokenType::Identifier = self.peek().r#type {
                return;
            }

            self.advance();
        }
    }

    fn error(&self, token: &Token, message: &str) -> Error {
        parser_error(token, message);
        Error::Parse {
            token: token.clone(),
            message: message.to_string(),
        }
    }
}

pub trait Interpreter<R>: Evaluate<R> {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<R, Error>;
}

pub trait Evaluate<R> {
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<R, Error>
    where
        Self: Sized,
        Self: StmtVisitor<R>,
    {
        stmt.accept(self)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<R, Error>
    where
        Self: Sized,
        Self: ExprVisitor<R>,
    {
        expr.accept(self)
    }
}

pub trait InterpreterErrors<R: fmt::Display> {
    fn runtime_error(&self, left: &R, op: &Token, right: &R) -> Result<R, Error> {
        let message = match op.r#type {
            TokenType::Minus
            | TokenType::Slash
            | TokenType::Star
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                format!("Operands must be numbers. Was: {} {} {}", left, op, right)
            }
            TokenType::Plus => {
                format!(
                    "Operands must be two numbers or two strings. Was: {} {} {}",
                    left, op, right
                )
            }
            _ => {
                format!("Invalid expr error. Was: {} {} {}", left, op, right)
            }
        };
        Err(Error::Runtime {
            token: op.clone(),
            message,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parse;
    use insta::assert_yaml_snapshot;

    macro_rules! test_parser {
        ($name:ident, $source:expr) => {
            #[test]
            fn $name() {
                let mut scanner = Lexer::new($source);
                let tokens = scanner.scan_tokens();
                let mut parser = Parser::new(tokens);
                assert_yaml_snapshot!(parser.parse().unwrap());
            }
        };
    }

    test_parser!(precedence_math, "15 - 3 * 4");
    test_parser!(grouping, "(1 + 2) * 3");
    test_parser!(parse_true, "true");
    test_parser!(parse_false, "false");
    test_parser!(parse_nil, "nil");
    test_parser!(parse_bang_equal, "1 != 2");
    test_parser!(parse_equal_equal, "1 == 2");
    test_parser!(parse_gt, "1 > 2");
    test_parser!(parse_ge, "1 >= 2");
    test_parser!(parse_lt, "1 < 2");
    test_parser!(parse_le, "1 <= 2");
}
