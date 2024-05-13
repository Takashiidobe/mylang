use crate::{
    lexer::{Op, Token, TokenType, Value},
    parser::{Error, Expr, Visitor},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expression: &Expr) -> Result<Value, Error> {
        expression.accept(self)
    }

    fn runtime_error(&self, left: &Value, operator: &Token, right: &Value) -> Result<Value, Error> {
        let message = match operator.r#type {
            TokenType::Op(Op::Minus)
            | TokenType::Op(Op::Slash)
            | TokenType::Op(Op::Star)
            | TokenType::Op(Op::Gt)
            | TokenType::Op(Op::Ge)
            | TokenType::Op(Op::Lt)
            | TokenType::Op(Op::Le) => {
                format!(
                    "Operands must be numbers. Was: {} {} {}",
                    left, operator, right
                )
            }
            TokenType::Op(Op::Plus) => {
                format!(
                    "Operands must be two numbers or two strings. Was: {} {} {}",
                    left, operator, right
                )
            }
            _ => {
                format!(
                    "Invalid expression error. Was: {} {} {}",
                    left, operator, right
                )
            }
        };
        Err(Error::Runtime {
            token: operator.clone(),
            message,
        })
    }
}

impl Visitor<Value> for Interpreter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Value, Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match (&left, &operator.r#type, &right) {
            (Value::Number(left_num), TokenType::Op(Op::Minus), Value::Number(right_num)) => {
                Ok(Value::Number(left_num - right_num))
            }
            (Value::Number(left_num), TokenType::Op(Op::Slash), Value::Number(0)) => {
                Err(Error::Runtime {
                    token: operator.clone(),
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
            _ => self.runtime_error(&left, operator, &right),
        }
    }

    fn visit_literal_expr(&self, value: &Value) -> Result<Value, Error> {
        Ok(value.clone())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        self.evaluate(expr)
    }
}
