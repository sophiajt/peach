//! Peach - a lightweight Rust *thing*
//!
//! Peach has three modes:
//!   * "build" - builds given project to a binary (uses the system C compiler)
//!   * "run" - converts the project to bytecode, which it runs immediately
//!   * "repl" - creates a repl to interact with the code directly

extern crate peach;
extern crate syn;

use peach::{compile_bytecode, Bytecode, BytecodeEngine, EvalEngine, VarStack};

use std::collections::HashMap;
use std::path::Path;

/// Run a peach repl on the commandline.
pub fn repl() {
    use std::io::{stdin, stdout, Write};

    let mut bc = BytecodeEngine::new();
    let mut ee = EvalEngine::new();
    let mut var_stack = VarStack::new();
    let mut var_lookup: HashMap<usize, usize> = HashMap::new();
    let mut show_type = false;
    let mut show_bytecode = false;

    println!("peach repl (:h for help, :q to quit)");
    loop {
        let mut bytecode: Vec<Bytecode> = vec![];
        let mut input = String::new();

        print!("> ");
        let _ = stdout().flush();
        stdin().read_line(&mut input).expect("Could not read input");
        input = input.trim().to_string();

        if input == ":quit" || input == ":q" {
            break;
        }

        if input == ":type" || input == ":t" {
            show_type ^= true;
            println!("show type: {}", show_type);
            continue;
        }

        if input == ":bytecode" || input == ":b" {
            show_bytecode ^= true;
            println!("show bytecode: {}", show_bytecode);
            continue;
        }

        if input == ":stack" || input == ":s" {
            println!("{:?}", ee.value_stack);
            continue;
        }

        if input == ":help" || input == ":h" {
            println!(":h(elp) - print this help message");
            println!(":t(ype) - print the result type");
            println!(":b(ytecode) - print the bytecode");
            println!(":s(tack) - print the contents of the stack");
            println!(":q(uit) - quit repl");
            continue;
        }

        if let Ok(type_id) = bc.process_raw_expr_str(&input, &mut bytecode, &mut var_stack) {
            if show_type {
                println!("type: {}", bc.typechecker.printable_name(type_id));
            }
            if show_bytecode {
                println!("bytecode: {:?}", bytecode);
            }
            ee.eval_block_bytecode(&bc, &bytecode, &mut var_lookup);

            // This funny little trick should, in theory, let us pop off temporaries without popping off our variables
            let last = if ee.value_stack.len() > var_lookup.len() {
                ee.value_stack.pop().unwrap()
            } else {
                ee.value_stack.last().unwrap().clone()
            };

            println!("{}", last);
        } else {
            match bc.process_raw_stmt_str(&input, &mut bytecode, &mut var_stack) {
                Ok(_) => {
                    if show_bytecode {
                        println!("bytecode: {:?}", bytecode);
                    }
                    ee.eval_block_bytecode(&bc, &bytecode, &mut var_lookup);
                }
                Err(_) => {
                    input.push(';');

                    match bc.process_raw_stmt_str(&input, &mut bytecode, &mut var_stack) {
                        Ok(_) => {
                            if show_bytecode {
                                println!("bytecode: {:?}", bytecode);
                            }
                            ee.eval_block_bytecode(&bc, &bytecode, &mut var_lookup);
                        }
                        Err(e) => {
                            println!("Error: {}", e);
                        }
                    }
                }
            }
        }
    }
}

fn process(fname: &str, start_fn: &str) -> BytecodeEngine {
    let mut bc = BytecodeEngine::new();

    // Step 1: Load up the parsed file so that we can lazily convert it
    //TODO: FIXME: we should probably take &str or Path
    let path = Path::new(fname).canonicalize().unwrap();
    bc.set_project_root(path.parent().unwrap().to_str().unwrap());
    bc.load_file(path.file_name().unwrap().to_str().unwrap());

    // Step 2: Convert to bytecode from the given location
    // We assume the starting function is found in scope 0, the starting scope
    bc.process_fn(start_fn, 0);
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
            let compile_result = compile_bytecode(&bc, &fname);
            match compile_result {
                Ok(msg) => println!("\nCompile succeeded: {}", msg),
                Err(e) => println!("\nCompile failed: {}", e),
            }
        }
        (Some(ref cmd), Some(ref fname)) if cmd == "run" => {
            let bc = process(&fname, "main");
            let mut ee = EvalEngine::new();

            println!("Eval result:");
            ee.eval_program(&bc, "main");
        }
        (Some(ref cmd), _) if cmd == "repl" => {
            repl();
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
}
