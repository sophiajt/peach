#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

#[macro_use]
extern crate structopt;

use std::collections::HashMap;

use std::fs::File;
use std::io::Read;

mod bytecode;
mod compile;
mod eval;
mod tests;

use bytecode::{BytecodeEngine, Bytecode, Context};
use eval::{eval_block_bytecode, Value};

use std::path::PathBuf;
use structopt::StructOpt;

/// madness
#[derive(StructOpt, Debug)]
#[structopt(name = "madness")]
struct Opt {
    /// Activate repl mode
    #[structopt(long = "repl")]
    repl: Option<String>,

    /// Activate repl mode
    #[structopt(long = "build")]
    build: bool,

    /// Activate repl mode
    #[structopt(long = "run")]
    run: bool,

    /// Function to start in
    #[structopt(long = "start", default_value = "main")]
    start_fn: String,

    /// Files to process
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

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
    let opt = Opt::from_args();

    if opt.build {
        for file in &opt.files {
            let fname = file.to_str().unwrap();
            let bc = process(fname, &opt.start_fn);
            let compile_result = compile::compile_bytecode(&bc, fname);
            println!("\nCompile result: {:?}", compile_result);
        }
    } else if opt.run {
        for file in &opt.files {
            let bc = process(file.to_str().unwrap(), &opt.start_fn);
            println!("Eval result:");
            eval::eval_engine(&bc, &opt.start_fn, &mut None);
        }
    } else if let Some(repl) = opt.repl {
        let result = syn::parse_str::<syn::Expr>(&repl);
        let mut bc = BytecodeEngine::new();
        let mut bytecode: Vec<Bytecode> = vec![];
        let mut context = bytecode::Context::new();
        let mut var_lookup: HashMap<usize, usize> = HashMap::new();
        let mut value_stack: Vec<Value> = vec![];

        println!("repl: {}", repl);

        bc.convert_expr_to_bytecode(&result.unwrap(), &bytecode::Ty::Unknown, &mut bytecode, &mut context);
        println!("{:?}", bytecode);

        eval_block_bytecode(&bc, &bytecode, &mut var_lookup, &mut value_stack, &mut None);

        println!("{:?}", value_stack.pop());
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
