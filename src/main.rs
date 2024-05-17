use std::env::args;
use std::error::Error;

use mylang::asm_interpreter::Codegen;
use mylang::ast_printer::AstPrinter;
use mylang::bc_compiler::BcCompiler;
use mylang::bc_interpreter::BcInterpreter;
use mylang::interpreter::AstInterpreter;
use mylang::parser::{Interpreter, Parse};
use mylang::{lexer::Lexer, parser::Parser};
use rustyline::DefaultEditor;

#[derive(Default)]
enum Mode {
    Ast,
    Asm,
    Bc,
    #[default]
    Repl,
}

fn main() -> Result<(), Box<dyn Error>> {
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

    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(s) => {
                let mut lexer = Lexer::new(&s);
                let tokens = lexer.scan_tokens();
                let mut parser = Parser::new(tokens.clone());
                let stmts = parser.parse();
                match mode {
                    Mode::Ast => {
                        let res = AstPrinter.interpret(&stmts?);
                        match res {
                            Ok(output) => println!("Output: {}", output),
                            Err(e) => eprintln!("Error: {}", e),
                        }
                    }
                    Mode::Bc => {
                        let mut interpreter = BcInterpreter::default();
                        let _ = interpreter.interpret(&stmts?);
                        let BcInterpreter { ops } = interpreter;
                        let mut compiler = BcCompiler::new(ops);
                        let res = compiler.compile();
                        if let Some(val) = res {
                            println!("{}", val);
                        }
                    }
                    Mode::Repl => {
                        let mut interpreter = AstInterpreter::default();
                        let _ = interpreter.interpret(&stmts?);
                    }
                    Mode::Asm => {
                        let mut codegen = Codegen::new();
                        let instructions = codegen.program(&stmts?);
                        for instruction in instructions {
                            println!("{}", instruction);
                        }
                    }
                }
            }
            Err(err) => {
                println!("Error: {:?}", err);
                return Ok(());
            }
        }
    }
}
