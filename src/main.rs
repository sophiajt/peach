#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

use std::collections::HashMap;

use std::fs::File;
use std::io::Read;

mod bytecode;
mod compile;
mod eval;
mod tests;

use bytecode::{Bytecode, BytecodeEngine};
use eval::{eval_block_bytecode, Value};

fn process(fname: &str, start_fn: &str) -> BytecodeEngine {
    let mut file = File::open(fname).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let mut bc = BytecodeEngine::new();

    // Step 1: Load up the parsed file so that we can lazily convert it
    bc.load_file(&src);

    // Step 2: Convert to bytecode from the given location
    bc.process(start_fn);
    //println!("{:#?}", bc.processed_fns);

    bc
}

fn main() {
    use std::env;

    let mut args = env::args();
    let _ = args.next(); // executable name

    match (args.next(), args.next()) {
        (Some(ref cmd), Some(ref fname)) if cmd == "build" => {
            let bc = process(&fname, "main");
            let compile_result = compile::compile_bytecode(&bc, &fname);
            println!("\nCompile result: {:?}", compile_result);
        }
        (Some(ref cmd), Some(ref fname)) if cmd == "run" => {
            let bc = process(&fname, "main");
            println!("Eval result:");
            eval::eval_engine(&bc, "main", &mut None);
        }
        (Some(ref cmd), _) if cmd == "repl" => {
            use std::io::{stdin, stdout, Write};

            let mut bc = BytecodeEngine::new();
            let mut context = bytecode::Context::new();
            let mut var_lookup: HashMap<usize, usize> = HashMap::new();
            let mut value_stack: Vec<Value> = vec![];

            loop {
                let mut bytecode: Vec<Bytecode> = vec![];
                let mut input = String::new();

                print!("> ");
                let _ = stdout().flush();
                stdin().read_line(&mut input).expect("Could not read input");
                input = input.trim().to_string();

                if input == "quit" {
                    break;
                }

                let result = syn::parse_str::<syn::Expr>(&input);
                match result {
                    Ok(expr) => {
                        //println!("expr: {:?}", expr);
                        bc.convert_expr_to_bytecode(
                            &expr,
                            &bytecode::Ty::Unknown,
                            &mut bytecode,
                            &mut context,
                        );
                        println!("{:?}", bytecode);
                        eval_block_bytecode(
                            &bc,
                            &bytecode,
                            &mut var_lookup,
                            &mut value_stack,
                            &mut None,
                        );
                        if value_stack.len() > 0 {
                            println!("{:?}", value_stack.pop().unwrap());
                        }
                    }
                    _ => {
                        if input.chars().rev().next() != Some(';') {
                            input.push(';');
                        }
                        let result = syn::parse_str::<syn::Stmt>(&input);
                        bc.convert_stmt_to_bytecode(
                            &result.unwrap(),
                            &bytecode::Ty::Unknown,
                            &mut bytecode,
                            &mut context,
                        );
                        println!("{:?}", bytecode);
                        eval_block_bytecode(
                            &bc,
                            &bytecode,
                            &mut var_lookup,
                            &mut value_stack,
                            &mut None,
                        );
                    }
                }
            }
        }
        (Some(ref cmd), _) => {
            println!("Unknown command: {}", cmd);
        }
        (None, _) => {
            println!("Usage:");
            println!("   build <filename>");
            println!("   run <filename>");
            println!("   repl");
        }
    }

    /*
    use std::env;

    let mut args = env::args();
    let _ = args.next(); // executable name

    for arg in args {
        let mut file = File::open(&arg).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let mut bc = BytecodeEngine::new();

        // Step 1: Load up the parsed file so that we can lazily convert it
        bc.load_file(&src);

        // Step 2: Convert to bytecode from the given location
        bc.process("main");
        //println!("{:#?}", bc.processed_fns);

        println!("Eval result:");
        eval::eval_engine(&bc, "main", &mut None);
        let compile_result = compile::compile_bytecode(&bc, &arg);
        println!("\nCompile result: {:?}", compile_result);
    }
    */
}
