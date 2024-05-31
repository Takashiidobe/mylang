use crate::{
    error::{parser_error, Error},
    expr::{Expr, ExprVisitor},
    stmt::{Stmt, StmtVisitor},
    token::{ObjType, Object, Token, TokenType},
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
    fn return_stmt(&mut self) -> Result<Stmt, Error>;
    fn for_stmt(&mut self) -> Result<Stmt, Error>;
    fn if_stmt(&mut self) -> Result<Stmt, Error>;
    fn print_stmt(&mut self) -> Result<Stmt, Error>;
    fn while_stmt(&mut self) -> Result<Stmt, Error>;
    fn function(&mut self, kind: &str) -> Result<Stmt, Error>;
    fn block(&mut self) -> Result<Vec<Stmt>, Error>;
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
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, Error>;
    fn call(&mut self) -> Result<Expr, Error>;
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
        let stmt = if self.r#match(&[TokenType::Fun]) {
            self.function("function")
        } else if self.r#match(&[TokenType::Var]) {
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
        if self.r#match(&[TokenType::For]) {
            self.for_stmt()
        } else if self.r#match(&[TokenType::Print]) {
            self.print_stmt()
        } else if self.r#match(&[TokenType::If]) {
            self.if_stmt()
        } else if self.r#match(&[TokenType::Return]) {
            self.return_stmt()
        } else if self.r#match(&[TokenType::While]) {
            self.while_stmt()
        } else if self.r#match(&[TokenType::LeftBrace]) {
            Ok(Stmt::Block {
                stmts: self.block()?,
            })
        } else {
            self.expr_stmt()
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, Error> {
        let keyword = self.previous();
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expr()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn for_stmt(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.")?;
        let initializer = if self.r#match(&[TokenType::Semicolon]) {
            None
        } else if self.r#match(&[TokenType::Var]) {
            Some(self.var_decl()?)
        } else {
            Some(self.expr_stmt()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expr()?)
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expr()?)
        } else {
            None
        };

        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.stmt()?;

        if let Some(expr) = increment {
            let inc_stmt = Stmt::Expr { expr };
            body = Stmt::Block {
                stmts: vec![body, inc_stmt],
            }
        }

        body = Stmt::While {
            cond: condition.unwrap_or(Expr::Literal {
                value: Object::Bool(true),
            }),
            body: Box::new(body),
        };

        if let Some(init_stmt) = initializer {
            body = Stmt::Block {
                stmts: vec![init_stmt, body],
            }
        }

        Ok(body)
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, Error> {
        let name = self.consume(&TokenType::Identifier, &format!("Expect {kind} name."))?;
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {kind} name."),
        )?;

        let mut params = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    self.error(&self.peek(), "Cannot have more than 255 parameters.");
                }
                params.push(self.consume(&TokenType::Identifier, "Expect parameter name.")?);

                if !self.r#match(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(&TokenType::RightParen, "Expect ')' after parameters.")?;

        self.consume(&TokenType::Minus, "Expect '-' after parameters.")?;
        self.consume(&TokenType::Greater, "Expect '>' after parameters.")?;

        let return_type = if self.r#match(&[TokenType::NumType]) {
            ObjType::Number
        } else if self.r#match(&[TokenType::StrType]) {
            ObjType::String
        } else if self.r#match(&[TokenType::BoolType]) {
            ObjType::Bool
        } else {
            ObjType::Nil
        };

        self.consume(
            &TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;

        let body = self.block()?;

        Ok(Stmt::Function {
            name,
            params,
            body,
            return_type,
        })
    }

    fn if_stmt(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let cond = self.expr()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;

        let then = Box::new(self.stmt()?);
        let r#else = if self.r#match(&[TokenType::Else]) {
            Box::new(Some(self.stmt()?))
        } else {
            Box::new(None)
        };

        Ok(Stmt::If { cond, then, r#else })
    }

    fn while_stmt(&mut self) -> Result<Stmt, Error> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let cond = self.expr()?;
        self.consume(&TokenType::RightParen, "Expect ')' after condition.")?;
        let body = Box::new(self.stmt()?);
        Ok(Stmt::While { cond, body })
    }

    fn print_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr()?;
        self.consume(&TokenType::Semicolon, "expected ';' after value.")?;
        Ok(Stmt::Print { expr })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.decl()?);
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
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

        self.call()
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, Error> {
        let mut arguments = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                arguments.push(self.expr()?);
                if arguments.len() >= 255 {
                    self.error(&self.peek(), "Can't have more than 255 arguments.");
                }
                if !self.r#match(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(&TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }

    fn call(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.r#match(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
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

    test_parser!(precedence_math, "15 - 3 * 4;");
    test_parser!(grouping, "(1 + 2) * 3;");
    test_parser!(parse_true, "true;");
    test_parser!(parse_false, "false;");
    test_parser!(parse_nil, "nil;");
    test_parser!(parse_bang_equal, "1 != 2;");
    test_parser!(parse_equal_equal, "1 == 2;");
    test_parser!(parse_gt, "1 > 2;");
    test_parser!(parse_ge, "1 >= 2;");
    test_parser!(parse_lt, "1 < 2;");
    test_parser!(parse_le, "1 <= 2;");
}
