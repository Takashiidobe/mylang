#[cfg(test)]
use serde::{Deserialize, Serialize};

use crate::{
    error::Error,
    expr::Expr,
    token::{ObjType, Token},
};

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
    Block {
        stmts: Vec<Stmt>,
    },
    Print {
        expr: Expr,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
        return_type: ObjType,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
    If {
        cond: Expr,
        then: Box<Stmt>,
        r#else: Box<Option<Stmt>>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
}

pub trait StmtVisitor<R> {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<R, Error>;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<R, Error>;
    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then: &Stmt,
        r#else: &Option<Stmt>,
    ) -> Result<R, Error>;
    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt) -> Result<R, Error>;
    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> Result<R, Error>;
    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
        return_type: &ObjType,
    ) -> Result<R, Error>;
    fn visit_return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) -> Result<R, Error>;
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &mut dyn StmtVisitor<R>) -> Result<R, Error> {
        match self {
            Stmt::Block { stmts } => visitor.visit_block_stmt(stmts),
            Stmt::Expr { expr } => visitor.visit_expr_stmt(expr),
            Stmt::Var { name, initializer } => visitor.visit_var_stmt(name, initializer),
            Stmt::Print { expr } => visitor.visit_print_stmt(expr),
            Stmt::If { cond, then, r#else } => visitor.visit_if_stmt(cond, then, r#else),
            Stmt::While { cond, body } => visitor.visit_while_stmt(cond, body),

            Stmt::Function {
                name,
                params,
                body,
                return_type,
            } => visitor.visit_function_stmt(name, params, body, return_type),
            Stmt::Return { keyword, value } => visitor.visit_return_stmt(keyword, value),
        }
    }
}
