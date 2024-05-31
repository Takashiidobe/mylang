use crate::{
    error::Error,
    expr::{Expr, ExprVisitor},
    parser::{Evaluate, Interpreter, InterpreterErrors},
    stmt::{Stmt, StmtVisitor},
    token::{ObjType, Object, Token, TokenType},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Constant(Object),
    Load(Token),
    Store(Token, Object),
    Print,
    Negate,
    Plus,
    Sub,
    Mul,
    Div,
    Gt,
    Ge,
    Lt,
    Le,
    Ne,
    EqEq,
    And,
    Or,
    Jump(usize),
    JumpIfFalse(usize),
    Pop,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct BcInterpreter {
    pub ops: Vec<Opcode>,
}

impl Interpreter<()> for BcInterpreter {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }
}

impl Evaluate<()> for BcInterpreter {}
impl Evaluate<Object> for BcInterpreter {}
impl InterpreterErrors<Object> for BcInterpreter {}

impl StmtVisitor<()> for BcInterpreter {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)
    }

    fn visit_var_stmt(&mut self, name: &Token, initializer: &Option<Expr>) -> Result<(), Error> {
        if let Some(init) = initializer {
            self.eval_expr(init)?;
        } else {
            self.ops.push(Opcode::Constant(Object::Nil));
        }

        let top = self.ops.pop();
        if let Some(Opcode::Constant(val)) = top {
            self.ops.push(Opcode::Store(name.clone(), val));
        } else {
            panic!("Invalid variable");
        }
        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)?;
        self.ops.push(Opcode::Print);
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then: &Stmt,
        r#else: &Option<Stmt>,
    ) -> Result<(), Error> {
        // eval the cond
        self.eval_expr(cond)?;
        // we want to jump over the if branch by this amount if false (take the else)
        self.ops.push(Opcode::JumpIfFalse(2));
        self.ops.push(Opcode::Pop);
        // otherwise, take the true branch.
        self.eval_stmt(then)?;
        // If there is an else, we want to eval it and skip over it if the top was true
        if let Some(else_branch) = r#else {
            self.ops.push(Opcode::Jump(2));
            self.ops.push(Opcode::Pop);
            self.eval_stmt(else_branch)?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt) -> Result<(), Error> {
        todo!()
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
        return_type: &ObjType,
    ) -> Result<(), Error> {
        todo!()
    }

    fn visit_return_stmt(&mut self, keyword: &Token, value: &Option<Expr>) -> Result<(), Error> {
        todo!()
    }
}

impl ExprVisitor<()> for BcInterpreter {
    fn visit_binary_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), Error> {
        self.eval_expr(left)?;
        self.eval_expr(right)?;

        match &op.r#type {
            TokenType::Plus => {
                self.ops.push(Opcode::Plus);
                Ok(())
            }
            TokenType::Minus => {
                self.ops.push(Opcode::Sub);
                Ok(())
            }
            TokenType::Star => {
                self.ops.push(Opcode::Mul);
                Ok(())
            }
            TokenType::Slash => {
                self.ops.push(Opcode::Div);
                Ok(())
            }
            TokenType::Greater => {
                self.ops.push(Opcode::Gt);
                Ok(())
            }
            TokenType::GreaterEqual => {
                self.ops.push(Opcode::Ge);
                Ok(())
            }
            TokenType::Less => {
                self.ops.push(Opcode::Lt);
                Ok(())
            }
            TokenType::LessEqual => {
                self.ops.push(Opcode::Le);
                Ok(())
            }
            TokenType::BangEqual => {
                self.ops.push(Opcode::Ne);
                Ok(())
            }
            TokenType::EqualEqual => {
                self.ops.push(Opcode::EqEq);
                Ok(())
            }
            _ => panic!("Invalid binary expr: {:?} {} {:?}", left, op, right),
        }
    }

    fn visit_literal_expr(&mut self, value: &Object) -> Result<(), Error> {
        self.ops.push(Opcode::Constant(value.clone()));
        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)?;
        match &op.r#type {
            TokenType::Minus => {
                self.ops.push(Opcode::Negate);
            }
            _ => panic!("Invalid unary expr: {} {:?}", op, expr),
        }
        Ok(())
    }

    fn visit_logical_expr(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<(), Error> {
        self.eval_expr(left)?;
        self.eval_expr(right)?;

        match &op.r#type {
            TokenType::And => {
                self.ops.push(Opcode::And);
                Ok(())
            }
            TokenType::Or => {
                self.ops.push(Opcode::Or);
                Ok(())
            }
            _ => panic!("Invalid logical expr: {:?} {} {:?}", left, op, right),
        }
    }

    fn visit_assign_expr(&mut self, name: &Token, expr: &Expr) -> Result<(), Error> {
        self.eval_expr(expr)?;
        let top = self.ops.pop();
        if let Some(Opcode::Constant(val)) = top {
            self.ops.push(Opcode::Store(name.clone(), val));
        } else {
            panic!("Invalid variable");
        }
        Ok(())
    }

    fn visit_var_expr(&mut self, op: &Token) -> Result<(), Error> {
        self.ops.push(Opcode::Load(op.clone()));
        Ok(())
    }

    fn visit_call_expr(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<(), Error> {
        todo!()
    }
}
