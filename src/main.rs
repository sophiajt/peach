#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

use std::fs::File;
use std::io::Read;

mod bytecode;
mod codegen;
mod eval;
mod tests;

use bytecode::BytecodeEngine;

fn main() {
    use std::env;

    let mut args = env::args();
    let _ = args.next(); // executable name

    for arg in args {
        let mut file = File::open(&arg).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");
        let mut bc = BytecodeEngine::new();

        // Step 1: Load up the parsed file so that we can lazily convert it
        bc.load_file(syntax_file);

        // Step 2: Convert to bytecode from the given location
        bc.process("main");
        //println!("{:#?}", bc.processed_fns);

        println!("Eval result:");
        eval::eval_engine(&bc, "main", &mut None);
        let compile_result = codegen::compile_bytecode(&bc, &arg);
        println!("\nCompile result: {:?}", compile_result);
    }
}
