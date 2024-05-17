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
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<String, Error> {
        let mut s = String::default();
        s.push_str(&expr.accept(self)?);
        Ok(s)
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
