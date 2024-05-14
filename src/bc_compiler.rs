use crate::lexer::{Op, Value};

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
