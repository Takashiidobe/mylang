use crate::{
    lexer::{Op, Token, TokenType, Value},
    parser::Expr,
};

pub struct Codegen {
    depth: i64,
}

impl Codegen {
    pub fn new() -> Self {
        Self { depth: 0 }
    }

    pub fn program(&mut self, expr: &Expr) {
        println!("  .text");
        println!(".LC0:");
        println!("  .string \"%d\\n\"");
        println!("  .globl main");
        println!("main:");
        self.expr(expr);
        println!("  push %rsp");
        println!("  mov %rax, %rsi");
        println!("  mov $.LC0, %rdi");
        println!("  xor %rax, %rax");
        println!("  call printf");
        println!("  xor %rax, %rax");
        println!("  pop %rsp");
        println!("  ret");
    }

    fn push(&mut self) {
        println!("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, arg: &str) {
        println!("  pop {}", arg);
        self.depth -= 1;
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal { value } => match value {
                Value::Number(val) => println!("  mov ${}, %rax", val),
                _ => todo!(),
            },
            Expr::Binary { left, op, right } => match op {
                Token {
                    r#type: TokenType::Op(Op::Slash),
                    ..
                } => {
                    self.expr(right.as_ref());
                    self.push();
                    self.expr(left.as_ref());
                    self.pop("%rdi");
                    println!("  cqo");
                    println!("  idiv %rdi, %rax");
                }
                Token {
                    r#type: TokenType::Op(Op::Plus),
                    ..
                } => {
                    self.expr(right.as_ref());
                    self.push();
                    self.expr(left.as_ref());
                    self.pop("%rdi");
                    println!("  add %rdi, %rax");
                }
                Token {
                    r#type: TokenType::Op(Op::Minus),
                    ..
                } => {
                    self.expr(right.as_ref());
                    self.push();
                    self.expr(left.as_ref());
                    self.pop("%rdi");
                    println!("  sub %rdi, %rax");
                }
                Token {
                    r#type: TokenType::Op(Op::Star),
                    ..
                } => {
                    self.expr(right.as_ref());
                    self.push();
                    self.expr(left.as_ref());
                    self.pop("%rdi");
                    println!("  imul %rdi, %rax");
                }
                _ => todo!(),
            },
            Expr::Grouping { expr } => self.expr(expr),
        };
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}
