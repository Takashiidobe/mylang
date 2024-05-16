#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{error::Error, expr::Expr};

#[cfg_attr(test, derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr { expr: Box<Expr> },
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<R, Error>;
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &mut dyn StmtVisitor<R>) -> Result<R, Error> {
        match self {
            Stmt::Expr { expr } => visitor.visit_expr_stmt(expr),
        }
    }
}
