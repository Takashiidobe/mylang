#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{
    error::Error,
    lexer::{Token, Value},
    parser::Visitor,
};

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

impl Expr {
    pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> Result<R, Error> {
        match self {
            Expr::Binary { left, op, right } => visitor.visit_binary_expr(left, op, right),
            Expr::Literal { value } => visitor.visit_literal_expr(value),
            Expr::Grouping { expr } => visitor.visit_grouping_expr(expr),
        }
    }
}
