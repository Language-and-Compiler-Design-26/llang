use lalrpop_util::lalrpop_mod;
use std::io::{self, BufRead, Write};

use std::rc::Rc;
use environment::Env;
use crate::interpreter::Eval;
use crate::typing::TypeCheck;
use crate::codegen::Compile;

mod ast;
mod symbols;
mod interpreter;
mod environment;
mod ast_typed;
mod typing;
mod codegen;
mod llvm_utils;
mod llvm_generator;

lalrpop_mod!(pub grammar);
fn main() {
    let parser = grammar::CondParser::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    println!("Welcome to the L language REPL & Compiler! (in Rust)");

    loop {
        print!("> ");
        stdout.flush().unwrap();

        let mut line = String::new();
        if stdin.lock().read_line(&mut line).unwrap() == 0 {
            break;
        }

        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if line == "exit" || line == "quit" {
            break;
        }

        match parser.parse(line) {
            Ok(expr) => 
                match expr.type_check(Rc::new(Env::new())) {
                    Ok(typed) => {
                        match expr.eval(Rc::new(Env::new())) {
                            Ok(val) => {
                                println!("{:?} = {:?}", expr, val);
                                match typed.compile() {
                                    Ok(cu) => {

                                        llvm_utils::print_compilation_unit(&cu);

                                        match llvm_generator::llvm_as_string(&cu) {
                                            Ok(ir) => println!("{}", ir),
                                            Err(err) => println!("failed to produce llvm code: {}", err),
                                        }

                                        match llvm_generator::write_llvm_text_file(&cu, "output.ll") {
                                            Ok(()) => println!("wrote LLVM IR to output.ll"),
                                            Err(err) => println!("failed to write output.ll: {}", err),
                                        }

                                        match llvm_generator::write_llvm_bitcode_to_file(&cu, "output.bc") {
                                            Ok(()) => println!("wrote LLVM IR to output.bc"),
                                            Err(err) => println!("failed to write output.bc: {}", err),
                                        }
                                    },
                                    Err(err) => println!("compile error: {}", err)
                                }
                            },
                            Err(err) => println!("Execution Error: {}", err)
                        }
                    },
                    Err(err) => println!("type error: {}", err)
                },
                Err(err) => println!("parse error: {}", err),
        }
    }
}


#[cfg(test)]
mod tests;
