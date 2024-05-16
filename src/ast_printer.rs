use crate::{
    error::Error,
    expr::{Expr, ExprVisitor},
    lexer::{Token, Value},
    parser::{Evaluate, Interpreter},
    stmt::{Stmt, StmtVisitor},
};

impl Interpreter<String> for AstPrinter {
    fn interpret(&mut self, stmts: &[Stmt]) -> Result<String, Error> {
        let mut s = String::default();
        for stmt in stmts {
            s.push_str(&self.eval_stmt(stmt)?);
            s.push('\n');
        }
        Ok(s)
    }
}

impl Evaluate<String> for AstPrinter {}

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, stmts: &[Stmt]) -> Result<String, Error> {
        let mut s = String::new();
        for stmt in stmts {
            match stmt {
                Stmt::Expr { .. } => {
                    s.push_str(&stmt.accept(self)?);
                    s.push('\n');
                }
            }
        }
        s.pop();
        Ok(s)
    }

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
        expr.accept(self)
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

    fn visit_literal_expr(&mut self, value: &Value) -> Result<String, Error> {
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

    fn visit_statement_expr(&mut self, expr: &Expr) -> Result<String, Error> {
        expr.accept(self)
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
                let tokens = scanner.lex();
                let mut parser = Parser::new(tokens);
                assert_yaml_snapshot!(AstPrinter.print(&parser.parse().unwrap()));
            }
        };
    }

    test_printer!(precedence_math, "15 - 3 * 4");
    test_printer!(parse_true, "true");
    test_printer!(parse_false, "false");
    test_printer!(parse_nil, "nil");
    test_printer!(parse_grouping, "(1 + 2) * 3");
}
