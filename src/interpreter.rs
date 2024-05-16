use crate::{
    error::Error,
    expr::{Expr, ExprVisitor},
    lexer::{Op, Token, TokenType, Value},
    parser::{Evaluate, Interpreter, InterpreterErrors},
    stmt::{Stmt, StmtVisitor},
};

#[derive(Debug, Clone, PartialEq)]
pub struct AstInterpreter;

impl Interpreter<Value> for AstInterpreter {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<Value, Error> {
        let mut values = vec![];
        for stmt in stmts {
            values.push(self.eval_stmt(stmt)?);
        }
        Ok(values.pop().unwrap())
    }
}
impl Evaluate<Value> for AstInterpreter {}
impl InterpreterErrors<Value> for AstInterpreter {}

impl StmtVisitor<Value> for AstInterpreter {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.visit_statement_expr(expr)
    }
}

impl ExprVisitor<Value> for AstInterpreter {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, Error> {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

        match (&left, &op.r#type, &right) {
            (Value::Number(left_num), TokenType::Op(Op::Minus), Value::Number(right_num)) => {
                Ok(Value::Number(left_num - right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Slash), Value::Number(0)) => {
                Err(Error::Runtime {
                    token: op.clone(),
                    message: format!("Zero division error. Tried to divide {} by 0.", left_num),
                })
            }
            (Value::Number(left_num), TokenType::Op(Op::Slash), Value::Number(right_num)) => {
                Ok(Value::Number(left_num / right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Star), Value::Number(right_num)) => {
                Ok(Value::Number(left_num * right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Plus), Value::Number(right_num)) => {
                Ok(Value::Number(left_num + right_num))
            }
            (Value::String(left_str), TokenType::Op(Op::Plus), Value::String(right_str)) => {
                Ok(Value::String(left_str.to_owned() + right_str))
            }
            (Value::Number(left_num), TokenType::Op(Op::Lt), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num < right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Le), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num <= right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Gt), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num > right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Ge), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num >= right_num))
            }
            (_, TokenType::Op(Op::BangEqual), _) => Ok(Value::Bool(left != right)),
            (_, TokenType::Op(Op::EqualEqual), _) => Ok(Value::Bool(left != right)),
            _ => self.runtime_error(&left, op, &right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<Value, Error> {
        Ok(value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.eval_expr(expr)
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<Value, Error> {
        let expr = self.eval_expr(expr)?;
        match (&op.r#type, &expr) {
            (TokenType::Op(Op::Plus), Value::Number(num)) => Ok(Value::Number(*num)),
            (TokenType::Op(Op::Minus), Value::Number(num)) => Ok(Value::Number(-num)),
            _ => panic!("Unexpected op for expr: {} {}", &op, &expr),
        }
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Value, Error> {
        match (left, &op.r#type, right) {
            (
                Expr::Literal {
                    value: Value::Bool(l),
                },
                TokenType::Op(Op::And),
                Expr::Literal {
                    value: Value::Bool(r),
                },
            ) => Ok(Value::Bool(*l && *r)),
            (
                Expr::Literal {
                    value: Value::Bool(l),
                },
                TokenType::Op(Op::Or),
                Expr::Literal {
                    value: Value::Bool(r),
                },
            ) => Ok(Value::Bool(*l || *r)),
            (_, TokenType::Op(Op::And), _) => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                if l.is_truthy() && r.is_truthy() {
                    Ok(r)
                } else {
                    Ok(l)
                }
            }
            (_, TokenType::Op(Op::Or), _) => {
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

    fn visit_statement_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.eval_expr(expr)
    }
}
