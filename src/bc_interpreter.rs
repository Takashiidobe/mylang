use crate::{
    error::Error,
    expr::Expr,
    lexer::{Op, Token, TokenType, Value},
    parser::{Interpreter, InterpreterErrors, Visitor},
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcInterpreter {
    pub ops: Vec<Op>,
    pub stack: Vec<Value>,
}

impl InterpreterErrors<Value> for BcInterpreter {}

impl Interpreter<()> for BcInterpreter {}

impl Visitor<()> for BcInterpreter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<(), Error> {
        self.evaluate(left)?;
        self.evaluate(right)?;

        match &operator.r#type {
            TokenType::Op(Op::Plus) => {
                self.ops.push(Op::Plus);
                Ok(())
            }
            TokenType::Op(Op::Minus) => {
                self.ops.push(Op::Minus);
                Ok(())
            }
            TokenType::Op(Op::Star) => {
                self.ops.push(Op::Star);
                Ok(())
            }
            TokenType::Op(Op::Slash) => {
                self.ops.push(Op::Slash);
                Ok(())
            }
            _ => panic!("Parse error"),
        }
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<(), Error> {
        self.stack.push(value.clone());
        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        self.evaluate(expr)
    }
}
