use crate::{bc_interpreter::Opcode, lexer::Value};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcCompiler {
    pub ops: Vec<Opcode>,
    pub stack: Vec<Value>,
}

impl BcCompiler {
    pub fn new(ops: Vec<Opcode>) -> Self {
        Self { ops, stack: vec![] }
    }

    pub fn compile(&mut self) -> Option<Value> {
        for op in &self.ops {
            match op {
                Opcode::Mul => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l * r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Div => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l / r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Plus => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

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
                Opcode::Sub => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l - r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Negate => {
                    let val = self.stack.pop()?;
                    match val {
                        Value::Number(num) => {
                            self.stack.push(Value::Number(-num));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Constant(value) => {
                    self.stack.push(value.clone());
                }
                Opcode::Gt => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l > r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Ge => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l >= r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Lt => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l < r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Le => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l <= r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Ne => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l != r));
                        }
                        (Value::String(l), Value::String(r)) => {
                            self.stack.push(Value::Bool(l != r));
                        }
                        (Value::Bool(l), Value::Bool(r)) => {
                            self.stack.push(Value::Bool(l != r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::EqEq => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l == r));
                        }
                        (Value::String(l), Value::String(r)) => {
                            self.stack.push(Value::Bool(l == r));
                        }
                        (Value::Bool(l), Value::Bool(r)) => {
                            self.stack.push(Value::Bool(l == r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::And => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Bool(l), Value::Bool(r)) => {
                            self.stack.push(Value::Bool(l && r));
                        }
                        (l, r) => {
                            if l.is_truthy() && r.is_truthy() {
                                self.stack.push(l);
                            } else {
                                self.stack.push(r);
                            }
                        }
                    }
                }
                Opcode::Or => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Value::Bool(l), Value::Bool(r)) => {
                            self.stack.push(Value::Bool(l || r));
                        }
                        (l, r) => {
                            if l.is_truthy() {
                                self.stack.push(l);
                            } else {
                                self.stack.push(r);
                            }
                        }
                    }
                }
            }
        }
        self.stack.pop()
    }
}