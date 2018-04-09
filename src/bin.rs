//! Peach - a lightweight Rust *thing*
//!
//! Peach has three modes:
//!   * "build" - builds given project to a binary (uses the system C compiler)
//!   * "run" - converts the project to bytecode, which it runs immediately
//!   * "repl" - creates a repl to interact with the code directly

extern crate peachlib;
use peachlib::{compile_bytecode, eval_engine, repl, BytecodeEngine};

use std::path::Path;

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
            println!("Eval result:");
            eval_engine(&bc, "main", &mut None);
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
