use crate::{
    error::Error,
    expr::Expr,
    lexer::{Op, Token, TokenType, Value},
    parser::{Interpreter, InterpreterErrors, Visitor},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant(Value),
    Negate,
    Plus,
    Sub,
    Mul,
    Div,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcInterpreter {
    pub ops: Vec<Opcode>,
}

impl InterpreterErrors<Value> for BcInterpreter {}

impl Interpreter<()> for BcInterpreter {}

impl Visitor<()> for BcInterpreter {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), Error> {
        self.evaluate(left)?;
        self.evaluate(right)?;

        match &op.r#type {
            TokenType::Op(Op::Plus) => {
                self.ops.push(Opcode::Plus);
                Ok(())
            }
            TokenType::Op(Op::Minus) => {
                self.ops.push(Opcode::Sub);
                Ok(())
            }
            TokenType::Op(Op::Star) => {
                self.ops.push(Opcode::Mul);
                Ok(())
            }
            TokenType::Op(Op::Slash) => {
                self.ops.push(Opcode::Div);
                Ok(())
            }
            _ => panic!("Invalid binary expr: {:?} {} {:?}", left, op, right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<(), Error> {
        self.ops.push(Opcode::Constant(value.clone()));
        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        self.evaluate(expr)
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<(), Error> {
        self.evaluate(expr)?;
        match &op.r#type {
            TokenType::Op(Op::Minus) => {
                self.ops.push(Opcode::Negate);
            }
            _ => panic!("Invalid unary expr: {} {:?}", op, expr),
        }
        Ok(())
    }
}
