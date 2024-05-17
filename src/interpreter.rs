use std::collections::HashMap;

use crate::{
    error::{parser_error, Error},
    expr::{Expr, ExprVisitor},
    parser::{Evaluate, Interpreter, InterpreterErrors},
    stmt::{Stmt, StmtVisitor},
    token::{Object, Token, TokenType},
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct AstInterpreter {
    vars: HashMap<Token, Object>,
}

impl Interpreter<()> for AstInterpreter {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        let mut values = vec![];
        for stmt in stmts {
            values.push(self.eval_stmt(stmt)?);
        }
        Ok(())
    }
}

impl Evaluate<()> for AstInterpreter {}
impl Evaluate<Object> for AstInterpreter {}
impl InterpreterErrors<Object> for AstInterpreter {}

impl StmtVisitor<()> for AstInterpreter {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)?;
        Ok(())
    }

    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<(), Error> {
        if name.literal.is_some() {
            if let Some(init) = initializer {
                let val = self.eval_expr(init)?;
                self.vars.insert(name.clone(), val.clone());
                return Ok(());
            } else {
                self.vars.insert(name.clone(), Object::Nil);
            }
        } else {
            parser_error(name, "variable has no name");
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        let val = self.eval_expr(expr)?;
        println!("{}", val);
        Ok(())
    }
}

impl ExprVisitor<Object> for AstInterpreter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Object, Error> {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

        match (&left, &op.r#type, &right) {
            (Object::Number(left_num), TokenType::Minus, Object::Number(right_num)) => {
                Ok(Object::Number(left_num - right_num))
            }
            (Object::Number(left_num), TokenType::Slash, Object::Number(0.0)) => {
                Err(Error::Runtime {
                    token: op.clone(),
                    message: format!("Zero division error. Tried to divide {} by 0.", left_num),
                })
            }
            (Object::Number(left_num), TokenType::Slash, Object::Number(right_num)) => {
                Ok(Object::Number(left_num / right_num))
            }
            (Object::Number(left_num), TokenType::Star, Object::Number(right_num)) => {
                Ok(Object::Number(left_num * right_num))
            }
            (Object::Number(left_num), TokenType::Plus, Object::Number(right_num)) => {
                Ok(Object::Number(left_num + right_num))
            }
            (Object::String(left_str), TokenType::Plus, Object::String(right_str)) => {
                Ok(Object::String(left_str.to_owned() + right_str))
            }
            (Object::Number(left_num), TokenType::Less, Object::Number(right_num)) => {
                Ok(Object::Bool(left_num < right_num))
            }
            (Object::Number(left_num), TokenType::LessEqual, Object::Number(right_num)) => {
                Ok(Object::Bool(left_num <= right_num))
            }
            (Object::Number(left_num), TokenType::Greater, Object::Number(right_num)) => {
                Ok(Object::Bool(left_num > right_num))
            }
            (Object::Number(left_num), TokenType::GreaterEqual, Object::Number(right_num)) => {
                Ok(Object::Bool(left_num >= right_num))
            }
            (_, TokenType::BangEqual, _) => Ok(Object::Bool(left != right)),
            (_, TokenType::EqualEqual, _) => Ok(Object::Bool(left != right)),
            _ => self.runtime_error(&left, op, &right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Object) -> Result<Object, Error> {
        Ok(value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Object, Error> {
        self.eval_expr(expr)
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<Object, Error> {
        let expr = self.eval_expr(expr)?;
        match (&op.r#type, &expr) {
            (TokenType::Plus, Object::Number(num)) => Ok(Object::Number(*num)),
            (TokenType::Minus, Object::Number(num)) => Ok(Object::Number(-num)),
            _ => panic!("Unexpected op for expr: {} {}", &op, &expr),
        }
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Object, Error> {
        match (left, &op.r#type, right) {
            (
                Expr::Literal {
                    value: Object::Bool(l),
                },
                TokenType::And,
                Expr::Literal {
                    value: Object::Bool(r),
                },
            ) => Ok(Object::Bool(*l && *r)),
            (
                Expr::Literal {
                    value: Object::Bool(l),
                },
                TokenType::Or,
                Expr::Literal {
                    value: Object::Bool(r),
                },
            ) => Ok(Object::Bool(*l || *r)),
            (_, TokenType::And, _) => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                if l.is_truthy() && r.is_truthy() {
                    Ok(r)
                } else {
                    Ok(l)
                }
            }
            (_, TokenType::Or, _) => {
                let l = self.eval_expr(left)?;

                if l.is_truthy() {
                    Ok(l)
                } else {
                    let r = self.eval_expr(right)?;
                    Ok(r)
                }
            }
            _ => panic!("unexpected logical op"),
        }
    }

    fn visit_assign_expr(&mut self, name: &Token, expr: &Expr) -> Result<Object, Error> {
        let val = self.eval_expr(expr)?;
        self.vars.insert(name.clone(), val.clone());
        Ok(val)
    }

    fn visit_var_expr(&mut self, op: &Token) -> Result<Object, Error> {
        if let Some(obj) = self.vars.get(op) {
            Ok(obj.clone())
        } else {
            Ok(Object::Nil)
        }
    }
}
