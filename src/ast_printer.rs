use crate::{
    error::Error,
    expr::{Expr, ExprVisitor},
    parser::{Evaluate, Interpreter},
    stmt::{Stmt, StmtVisitor},
    token::{Object, Token},
};

impl Interpreter<String> for AstPrinter {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<String, Error> {
        let mut s = String::default();
        for stmt in stmts {
            s.push_str(&self.eval_stmt(stmt)?);
            s.push('\n');
        }
        s.pop();
        Ok(s)
    }
}

impl Evaluate<String> for AstPrinter {}

pub struct AstPrinter;

impl AstPrinter {
    fn parenthesize(&mut self, name: String, exprs: &[&Expr]) -> Result<String, Error> {
        let mut r = String::new();
        r.push('(');
        r.push_str(&name);
        for e in exprs {
            r.push(' ');
            r.push_str(&e.accept(self)?);
        }
        r.push(')');
        Ok(r)
    }

    fn parenthesize_stmts(&mut self, stmts: &[&Stmt]) -> Result<String, Error> {
        let mut s = String::new();
        for stmt in stmts {
            s.push_str(&self.eval_stmt(stmt)?);
            s.push('\n');
        }
        s.pop();
        Ok(s)
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<String, Error> {
        Ok(expr.accept(self)?.to_string())
    }

    fn visit_var_stmt(
        &mut self,
        name: &Token,
        initializer: &Option<Expr>,
    ) -> Result<String, Error> {
        if let Some(init) = initializer {
            self.parenthesize(format!("set {name}"), &[init])
        } else {
            self.parenthesize(format!("set {name}"), &[])
        }
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<String, Error> {
        self.parenthesize("print".to_string(), &[expr])
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then: &Stmt,
        r#else: &Option<Stmt>,
    ) -> Result<String, Error> {
        let mut s = String::new();
        s.push_str(&self.parenthesize("if".to_string(), &[cond])?);
        s.push_str(&self.eval_stmt(then)?);
        if let Some(else_cond) = r#else {
            s.push_str(&self.eval_stmt(else_cond)?);
        }
        Ok(s)
    }

    fn visit_while_stmt(&mut self, cond: &Expr, body: &Stmt) -> Result<String, Error> {
        let mut s = String::new();
        s.push_str(&self.parenthesize("while".to_string(), &[cond])?);
        s.push('\n');
        s.push_str(&self.parenthesize_stmts(&[body])?);
        Ok(s)
    }

    fn visit_block_stmt(&mut self, stmts: &[Stmt]) -> Result<String, Error> {
        let mut s = String::new();
        for stmt in stmts {
            s.push_str(&self.eval_stmt(stmt)?);
            s.push('\n');
        }
        s.pop();
        Ok(s)
    }

    fn visit_function_stmt(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Stmt],
    ) -> Result<String, Error> {
        todo!()
    }

    fn visit_return_stmt(
        &mut self,
        keyword: &Token,
        value: &Option<Expr>,
    ) -> Result<String, Error> {
        todo!()
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<String, Error> {
        self.parenthesize(op.to_string(), &[left, right])
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<String, Error> {
        self.parenthesize("group".to_string(), &[expr])
    }

    fn visit_literal_expr(&mut self, value: &Object) -> Result<String, Error> {
        Ok(value.to_string())
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<String, Error> {
        self.parenthesize(op.to_string(), &[expr])
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<String, Error> {
        self.parenthesize(op.to_string(), &[left, right])
    }

    fn visit_assign_expr(&mut self, name: &Token, expr: &Expr) -> Result<String, Error> {
        self.parenthesize(format!("set {name}"), &[expr])
    }

    fn visit_var_expr(&mut self, op: &Token) -> Result<String, Error> {
        Ok(op.to_string())
    }

    fn visit_call_expr(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arguments: &[Expr],
    ) -> Result<String, Error> {
        let mut aggregated = vec![callee];
        aggregated.extend(arguments.iter());
        self.parenthesize(paren.lexeme.clone(), &aggregated)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parse;
    use crate::parser::Parser;
    use insta::assert_yaml_snapshot;

    macro_rules! test_printer {
        ($name:ident, $source:expr) => {
            #[test]
            fn $name() {
                let mut scanner = Lexer::new($source);
                let tokens = scanner.scan_tokens();
                let mut parser = Parser::new(tokens);
                assert_yaml_snapshot!(AstPrinter.interpret(&parser.parse().unwrap()));
            }
        };
    }

    test_printer!(precedence_math, "15 - 3 * 4");
    test_printer!(parse_true, "true");
    test_printer!(parse_false, "false");
    test_printer!(parse_nil, "nil");
    test_printer!(parse_grouping, "(1 + 2) * 3");
}
