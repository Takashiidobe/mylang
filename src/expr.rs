#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{
    error::Error,
    lexer::{Token, Value},
};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Stmt {
        expr: Box<Expr>,
    },
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Literal {
        value: Value,
    },
    Grouping {
        expr: Box<Expr>,
    },
}

pub trait ExprVisitor<R> {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_literal_expr(&mut self, value: &Value) -> Result<R, Error>;
    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<R, Error>;
    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_statement_expr(&mut self, expr: &Expr) -> Result<R, Error>;
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn ExprVisitor<R>) -> Result<R, Error> {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Unary { op, expr } => visitor.visit_unary_expr(op, expr),
            Expr::Stmt { expr } => visitor.visit_statement_expr(expr),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
            Expr::Logical { left, op, right } => visitor.visit_logical_expr(left, op, right),
        }
    }
}
