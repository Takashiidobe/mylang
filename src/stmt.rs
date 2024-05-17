#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{error::Error, expr::Expr, token::Token};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr {
        expr: Expr,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    Print {
        expr: Expr,
    },
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<R, Error>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<R, Error>;
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &mut dyn StmtVisitor<R>) -> Result<R, Error> {
        match self {
            Stmt::Expr { expr } => visitor.visit_expr_stmt(expr),
            Stmt::Var { name, initializer } => visitor.visit_var_stmt(name, initializer),
            Stmt::Print { expr } => visitor.visit_print_stmt(expr),
        }
    }
}
