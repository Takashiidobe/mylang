#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{
    error::Error,
    token::{Object, Token},
};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Assign {
        name: Token,
        expr: Box<Expr>,
    },
    Var {
        name: Token,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Literal {
        value: Object,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

pub trait ExprVisitor<R> {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_literal_expr(&mut self, value: &Object) -> Result<R, Error>;
    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<R, Error>;
    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<R, Error>;
    fn visit_assign_expr(&mut self, name: &Token, expr: &Expr) -> Result<R, Error>;
    fn visit_var_expr(&mut self, op: &Token) -> Result<R, Error>;
    fn visit_call_expr(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<R, Error>;
}

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn ExprVisitor<R>) -> Result<R, Error> {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Unary { op, expr } => visitor.visit_unary_expr(op, expr),
            Expr::Assign { name, expr } => visitor.visit_assign_expr(name, expr),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
            Expr::Logical { left, op, right } => visitor.visit_logical_expr(left, op, right),
            Expr::Var { name } => visitor.visit_var_expr(name),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => visitor.visit_call_expr(callee, paren, arguments),
        }
    }
}
