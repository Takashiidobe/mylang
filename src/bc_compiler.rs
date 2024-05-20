use std::collections::HashMap;

use crate::{
    bc_interpreter::Opcode,
    token::{Object, Token},
};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcCompiler {
    pub ops: Vec<Opcode>,
    pub stack: Vec<Object>,
    pub vars: HashMap<Token, Object>,
    pub ip: usize,
}

impl BcCompiler {
    pub fn new(ops: Vec<Opcode>) -> Self {
        Self {
            ops,
            stack: vec![],
            vars: HashMap::default(),
            ip: 0,
        }
    }

    pub fn compile(&mut self) -> Option<()> {
        while self.ip < self.ops.len() {
            let op = &self.ops[self.ip];
            match op {
                Opcode::Mul => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Number(l * r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Div => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Number(l / r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Plus => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;

                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Number(l + r));
                        }
                        (Object::String(l), Object::String(r)) => {
                            let mut res = l.clone();
                            res.push_str(&r);
                            self.stack.push(Object::String(res));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Sub => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Number(l - r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Negate => {
                    let val = self.stack.pop()?;
                    match val {
                        Object::Number(num) => {
                            self.stack.push(Object::Number(-num));
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
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l > r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Ge => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l >= r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Lt => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l < r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Le => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l <= r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::Ne => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l != r));
                        }
                        (Object::String(l), Object::String(r)) => {
                            self.stack.push(Object::Bool(l != r));
                        }
                        (Object::Bool(l), Object::Bool(r)) => {
                            self.stack.push(Object::Bool(l != r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::EqEq => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Number(l), Object::Number(r)) => {
                            self.stack.push(Object::Bool(l == r));
                        }
                        (Object::String(l), Object::String(r)) => {
                            self.stack.push(Object::Bool(l == r));
                        }
                        (Object::Bool(l), Object::Bool(r)) => {
                            self.stack.push(Object::Bool(l == r));
                        }
                        _ => panic!("incorrect ops"),
                    }
                }
                Opcode::And => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    match (a, b) {
                        (Object::Bool(l), Object::Bool(r)) => {
                            self.stack.push(Object::Bool(l && r));
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
                        (Object::Bool(l), Object::Bool(r)) => {
                            self.stack.push(Object::Bool(l || r));
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
                Opcode::Print => {
                    let top = self.stack.pop()?;
                    println!("{}", top);
                }
                Opcode::Load(op) => {
                    if let Some(val) = self.vars.get(op) {
                        self.stack.push(val.clone());
                    } else {
                        panic!("variable {} was not found", op);
                    }
                }
                Opcode::Store(op, val) => {
                    self.vars.insert(op.clone(), val.clone());
                }
                Opcode::Jump(offset) => {
                    dbg!(&self.ops);
                    dbg!(offset);
                    println!("can jump by {}", offset);
                    self.ip += offset;
                }
                Opcode::JumpIfFalse(offset) => {
                    let top = self.stack.pop()?;
                    dbg!(&self.ops);
                    dbg!(offset);
                    dbg!(&top);
                    if !top.is_truthy() {
                        println!("can jump by {}", offset);
                        self.ip += offset;
                    }
                }
                Opcode::Pop => {
                    self.stack.pop()?;
                }
            }
            self.ip += 1;
        }
        Some(())
    }
}
