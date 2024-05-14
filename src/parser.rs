#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::lexer::Keyword;
use crate::lexer::Op;
use crate::lexer::Punct;
use crate::lexer::SourceLocation;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::Value;
use std::convert;
use std::fmt;
use std::io;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

pub trait Parse<P> {
    fn parse(&mut self) -> Result<P, Error>;
    fn expression(&mut self) -> Result<P, Error>;
    fn term(&mut self) -> Result<P, Error>;
    fn factor(&mut self) -> Result<P, Error>;
    fn primary(&mut self) -> Result<P, Error>;
}

impl Parse<Expr> for Parser {
    fn parse(&mut self) -> Result<Expr, Error> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.term()
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.r#match(&[TokenType::Op(Op::Minus), TokenType::Op(Op::Plus)]) {
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
        let mut expr = self.primary()?;

        while self.r#match(&[TokenType::Op(Op::Slash), TokenType::Op(Op::Star)]) {
            let op = self.previous();
            let right = self.primary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.peek();

        let expr = if self.r#match(&[TokenType::Keyword(Keyword::False)]) {
            Expr::Literal {
                value: Value::Bool(false),
            }
        } else if self.r#match(&[TokenType::Keyword(Keyword::True)]) {
            Expr::Literal {
                value: Value::Bool(true),
            }
        } else if self.r#match(&[TokenType::Keyword(Keyword::Nil)]) {
            Expr::Literal { value: Value::Nil }
        } else if self.r#match(&[TokenType::String, TokenType::Number]) {
            Expr::Literal {
                value: token.value.unwrap_or_default(),
            }
        } else if self.r#match(&[TokenType::Punct(Punct::LParen)]) {
            let expr = self.expression()?;
            self.consume(
                &TokenType::Punct(Punct::RParen),
                "Expected ')' after expression.",
            )?;
            Expr::Grouping {
                expr: Box::new(expr),
            }
        } else {
            return Err(Error::Runtime {
                token,
                message: "Expected expression".to_string(),
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
            if self.previous().r#type == TokenType::Punct(Punct::Semicolon) {
                return;
            }

            if let TokenType::Keyword(_) = self.peek().r#type {
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

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Parse { token: Token, message: String },
    Runtime { token: Token, message: String },
}

#[cfg(test)]
impl Serialize for Error {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(underlying) => write!(f, "IoError {}", underlying),
            Error::Parse { token, message } => {
                write!(f, "ParseError at token: {}, message: {}", token, message)
            }
            Error::Runtime { token, message } => {
                write!(f, "RuntimeError at token: {}, message: {}", token, message)
            }
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "Lox Error"
    }
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Literal {
        value: Value,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

pub trait Visitor<R> {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_literal_expr(&mut self, value: &Value) -> Result<R, Error>;
}

pub trait Interpreter<R: fmt::Display>: Visitor<R> {
    fn interpret(&mut self, expr: &Expr) -> Result<R, Error>
    where
        Self: Sized,
    {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expression: &Expr) -> Result<R, Error>
    where
        Self: Sized,
    {
        expression.accept(self)
    }

    fn runtime_error(&self, left: &R, operator: &Token, right: &R) -> Result<R, Error> {
        let message = match operator.r#type {
            TokenType::Op(Op::Minus)
            | TokenType::Op(Op::Slash)
            | TokenType::Op(Op::Star)
            | TokenType::Op(Op::Gt)
            | TokenType::Op(Op::Ge)
            | TokenType::Op(Op::Lt)
            | TokenType::Op(Op::Le) => {
                format!(
                    "Operands must be numbers. Was: {} {} {}",
                    left, operator, right
                )
            }
            TokenType::Op(Op::Plus) => {
                format!(
                    "Operands must be two numbers or two strings. Was: {} {} {}",
                    left, operator, right
                )
            }
            _ => {
                format!(
                    "Invalid expression error. Was: {} {} {}",
                    left, operator, right
                )
            }
        };
        Err(Error::Runtime {
            token: operator.clone(),
            message,
        })
    }
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> Result<R, Error> {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
        }
    }
}

pub fn report(loc: &SourceLocation, message: &str) {
    eprintln!("[line {}, col {}] Error: {}", loc.line, loc.col, message);
}

pub fn parser_error(token: &Token, message: &str) {
    report(&token.loc, message);
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
                let tokens = scanner.lex();
                let mut parser = Parser::new(tokens);
                assert_yaml_snapshot!(Parse::<Expr>::parse(&mut parser).unwrap());
            }
        };
    }

    test_parser!(precedence_math, "15 - 3 * 4");
    test_parser!(grouping, "(1 + 2) * 3");
    test_parser!(parse_true, "true");
    test_parser!(parse_false, "false");
    test_parser!(parse_nil, "nil");
}
