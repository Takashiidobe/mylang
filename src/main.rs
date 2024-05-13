use std::env::args;
use std::io::stdin;

use mylang::asm_interpreter::Codegen;
use mylang::ast_printer::AstPrinter;
use mylang::parser::Parse;
use mylang::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

#[derive(Default)]
enum Mode {
    Ast,
    Bytecode,
    Asm,
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
                mode = Mode::Bytecode;
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
        s.pop(); // remove newline
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
            Mode::Repl => {
                let res = Interpreter.interpret(&expr.unwrap());
                match res {
                    Ok(output) => println!("Output: {}", output),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Mode::Bytecode => {
                let res = Interpreter.interpret(&expr.unwrap());
                match res {
                    Ok(output) => println!("Output: {:?}", output),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Mode::Asm => {
                let mut codegen = Codegen::new();
                codegen.program(&expr.unwrap());
            }
        }

        s.clear();
    }
}
