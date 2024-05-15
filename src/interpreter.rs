use crate::{
    error::Error,
    expr::Expr,
    lexer::{Op, Token, TokenType, Value},
    parser::{Interpreter, InterpreterErrors, Visitor},
};

#[derive(Debug, Clone, PartialEq)]
pub struct AstInterpreter;

impl Interpreter<Value> for AstInterpreter {}
impl InterpreterErrors<Value> for AstInterpreter {}

impl Visitor<Value> for AstInterpreter {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value, Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

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
            (Value::Number(left_num), TokenType::Op(Op::BangEqual), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num != right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::EqualEqual), Value::Number(right_num)) => {
                Ok(Value::Bool(left_num == right_num))
            }
            _ => self.runtime_error(&left, op, &right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<Value, Error> {
        Ok(value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.evaluate(expr)
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<Value, Error> {
        let expr = self.evaluate(expr)?;
        match (&op.r#type, &expr) {
            (TokenType::Op(Op::Plus), Value::Number(num)) => Ok(Value::Number(*num)),
            (TokenType::Op(Op::Minus), Value::Number(num)) => Ok(Value::Number(-num)),
            _ => panic!("Unexpected op for expr: {} {}", &op, &expr),
        }
    }
}
