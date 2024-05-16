use crate::{
    error::Error,
    expr::{Expr, ExprVisitor},
    lexer::{Keyword, Op, Punct, SourceLocation, Token, TokenType, Value},
    stmt::{Stmt, StmtVisitor},
};
use std::fmt;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

pub trait Parse {
    fn parse(&mut self) -> Result<Vec<Stmt>, Error>;
    fn expr(&mut self) -> Result<Expr, Error>;
    fn expr_stmt(&mut self) -> Result<Expr, Error>;
    fn stmt(&mut self) -> Result<Stmt, Error>;
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
            stmts.push(self.stmt()?);
        }

        Ok(stmts)
    }

    fn stmt(&mut self) -> Result<Stmt, Error> {
        Ok(Stmt::Expr {
            expr: Box::new(self.expr_stmt()?),
        })
    }

    fn expr_stmt(&mut self) -> Result<Expr, Error> {
        let expr = Expr::Stmt {
            expr: Box::new(self.expr()?),
        };
        self.consume(&TokenType::Punct(Punct::Semicolon), "expected semicolon")?;
        Ok(expr)
    }

    fn expr(&mut self) -> Result<Expr, Error> {
        self.or()
    }

    fn or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.and()?;

        while self.r#match(&[TokenType::Op(Op::Or)]) {
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

        while self.r#match(&[TokenType::Op(Op::And)]) {
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

        while self.r#match(&[TokenType::Op(Op::BangEqual), TokenType::Op(Op::EqualEqual)]) {
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
            TokenType::Op(Op::Gt),
            TokenType::Op(Op::Ge),
            TokenType::Op(Op::Lt),
            TokenType::Op(Op::Le),
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
        let mut expr = self.unary()?;

        while self.r#match(&[TokenType::Op(Op::Slash), TokenType::Op(Op::Star)]) {
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
        if self.r#match(&[TokenType::Op(Op::Plus)]) {
            self.previous();
            return self.unary();
        }

        if self.r#match(&[TokenType::Op(Op::Minus)]) {
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
            let expr = self.expr()?;
            self.consume(&TokenType::Punct(Punct::RParen), "Expected ')' after expr.")?;
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
            TokenType::Op(Op::Minus)
            | TokenType::Op(Op::Slash)
            | TokenType::Op(Op::Star)
            | TokenType::Op(Op::Gt)
            | TokenType::Op(Op::Ge)
            | TokenType::Op(Op::Lt)
            | TokenType::Op(Op::Le) => {
                format!("Operands must be numbers. Was: {} {} {}", left, op, right)
            }
            TokenType::Op(Op::Plus) => {
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
