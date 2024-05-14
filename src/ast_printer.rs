use crate::{
    error::Error,
    expr::Expr,
    lexer::{Token, Value},
    parser::Visitor,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> Result<String, Error> {
        expr.accept(self)
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

impl Visitor<String> for AstPrinter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<String, Error> {
        self.parenthesize(operator.to_string(), &[left, right])
    }

    fn visit_grouping_expr(&mut self, expr: &Expr) -> Result<String, Error> {
        self.parenthesize("group".to_string(), &[expr])
    }

    fn visit_literal_expr(&mut self, value: &Value) -> Result<String, Error> {
        Ok(value.to_string())
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
