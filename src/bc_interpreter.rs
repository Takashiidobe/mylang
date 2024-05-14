use crate::{
    lexer::{Op, Token, TokenType, Value},
    parser::{Error, Expr, Interpreter, InterpreterErrors, Visitor},
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcInterpreter {
    pub ops: Vec<Op>,
    pub stack: Vec<Value>,
}

impl InterpreterErrors<Value> for BcInterpreter {}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcCompiler {
    pub ops: Vec<Op>,
    pub stack: Vec<Value>,
}

impl BcCompiler {
    pub fn new(ops: Vec<Op>, stack: Vec<Value>) -> Self {
        Self { ops, stack }
    }

    pub fn compile(&mut self) -> Value {
        for op in &self.ops {
            match op {
                Op::Star => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l * r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Op::Slash => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l / r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Op::Plus => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l + r));
                        }
                        (Value::String(l), Value::String(r)) => {
                            let mut res = l.clone();
                            res.push_str(&r);
                            self.stack.push(Value::String(res));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Op::Minus => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l - r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                _ => todo!(),
            }
        }
        self.stack.pop().unwrap()
    }
}

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
