use core::fmt;

use crate::{
    lexer::{Op, Token, TokenType, Value},
    parser::{Error, Expr, Interpreter, Visitor},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant(Value),
    Op(Op),
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Constant(val) => f.write_fmt(format_args!("{}", val)),
            Opcode::Op(op) => f.write_fmt(format_args!("{}", op)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BcInterpreter;

impl Interpreter<Opcode> for BcInterpreter {
    fn interpret(&mut self, expr: &Expr) -> Result<Opcode, Error> {
        self.evaluate(expr)
    }

    fn evaluate(&mut self, expression: &Expr) -> Result<Opcode, Error> {
        expression.accept(self)
    }

    fn runtime_error(
        &self,
        left: &Opcode,
        operator: &Token,
        right: &Opcode,
    ) -> Result<Opcode, Error> {
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

impl Visitor<Opcode> for BcInterpreter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<Opcode, Error> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match (&left, &operator.r#type, &right) {
            (
                Opcode::Constant(Value::Number(left_num)),
                TokenType::Op(Op::Plus),
                Opcode::Constant(Value::Number(right_num)),
            ) => Ok(Opcode::Constant(Value::Number(left_num + right_num))),
            (
                Opcode::Constant(Value::Number(left_num)),
                TokenType::Op(Op::Minus),
                Opcode::Constant(Value::Number(right_num)),
            ) => Ok(Opcode::Constant(Value::Number(left_num - right_num))),
            (
                Opcode::Constant(Value::Number(left_num)),
                TokenType::Op(Op::Star),
                Opcode::Constant(Value::Number(right_num)),
            ) => Ok(Opcode::Constant(Value::Number(left_num * right_num))),
            (
                Opcode::Constant(Value::Number(left_num)),
                TokenType::Op(Op::Slash),
                Opcode::Constant(Value::Number(0)),
            ) => Err(Error::Runtime {
                token: operator.clone(),
                message: format!("Zero division error. Tried to divide {} by 0.", left_num),
            }),
            (
                Opcode::Constant(Value::Number(left_num)),
                TokenType::Op(Op::Slash),
                Opcode::Constant(Value::Number(right_num)),
            ) => Ok(Opcode::Constant(Value::Number(left_num / right_num))),
            (
                Opcode::Constant(Value::String(left_str)),
                TokenType::Op(Op::Star),
                Opcode::Constant(Value::String(right_str)),
            ) => Ok(Opcode::Constant(Value::String(
                left_str.to_owned() + right_str,
            ))),
            _ => self.runtime_error(&left, operator, &right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<Opcode, Error> {
        Ok(Opcode::Constant(value.clone()))
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<Opcode, Error> {
        self.evaluate(expr)
    }
}
