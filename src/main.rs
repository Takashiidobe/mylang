use std::env::args;
use std::io::stdin;

use mylang::asm_interpreter::Codegen;
use mylang::ast_printer::AstPrinter;
use mylang::bc_interpreter::BcInterpreter;
use mylang::interpreter::AstInterpreter;
use mylang::parser::{Interpreter, Parse};
use mylang::{lexer::Lexer, parser::Parser};

#[derive(Default)]
enum Mode {
    Ast,
    Asm,
    Bc,
    #[default]
    Repl,
}

fn main() {
    let args: Vec<_> = args().collect();
    let mut mode = Mode::default();
    if args.len() > 1 {
        match args[1].as_str() {
            "ast" => {
                mode = Mode::Ast;
            }
            "bc" => {
                mode = Mode::Bc;
            }
            "asm" => {
                mode = Mode::Asm;
            }
            _ => {}
        }
    }

    let mut s = String::default();
    loop {
        stdin().read_line(&mut s).unwrap();
        s.pop();
        let mut lexer = Lexer::new(&s);
        let tokens = lexer.lex();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse();
        match mode {
            Mode::Ast => {
                let res = AstPrinter.print(&expr.unwrap());
                match res {
                    Ok(output) => println!("Output: {}", output),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Mode::Bc => {
                let res = BcInterpreter.interpret(&expr.unwrap());
                match res {
                    Ok(output) => println!("Output: {}", output),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Mode::Repl => {
                let res = AstInterpreter.interpret(&expr.unwrap());
                match res {
                    Ok(output) => println!("Output: {}", output),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Mode::Asm => {
                let mut codegen = Codegen::new();
                let instructions = codegen.program(&expr.unwrap());
                for instruction in instructions {
                    println!("{}", instruction);
                }
            }
        }

        s.clear();
    }
}
