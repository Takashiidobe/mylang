use std::borrow::Cow::{self, Borrowed, Owned};
use std::env::args;
use std::error::Error;

use mylang::asm_interpreter::Codegen;
use mylang::ast_printer::AstPrinter;
use mylang::bc_compiler::BcCompiler;
use mylang::bc_interpreter::BcInterpreter;
use mylang::interpreter::AstInterpreter;
use mylang::parser::{Interpreter, Parse};
use mylang::{lexer::Lexer, parser::Parser};

use rustyline::completion::FilenameCompleter;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::HistoryHinter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::{CompletionType, Config, EditMode, Editor};

#[derive(Default)]
enum Mode {
    Ast,
    Asm,
    Bc,
    #[default]
    Repl,
}

#[derive(Helper, Completer, Hinter, Validator)]
struct MyHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        self.highlighter.highlight_char(line, pos, forced)
    }
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

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Vi)
        .build();
    let h = MyHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter::new(),
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let mut count = 1;
    loop {
        let p = format!("{count}> ");
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{p}\x1b[0m");
        let readline = rl.readline(&p);
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
                        compiler.compile();
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
                rl.add_history_entry(s.as_str())?;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
        count += 1;
    }
    rl.append_history("history.txt")?;
    Ok(())
}
