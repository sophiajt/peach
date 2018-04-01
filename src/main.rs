#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

extern crate clap;
use clap::{App, Arg};

use std::fs::File;
use std::io::Read;

mod bytecode;
mod codegen;
mod eval;
mod tests;

use bytecode::BytecodeEngine;

/*
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
*/

fn main() {
    let matches = App::new("madness")
        .version("0.1")
        .arg(
            Arg::with_name("build")
                .help("Compile the given file")
                .short("b")
                .long("build"),
        )
        .arg(
            Arg::with_name("run")
                .help("Run the bytecode of the given file")
                .short("r")
                .long("run"),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("Files to run/compile")
                .required(true)
                .multiple(true)
                .index(1),
        )
        .get_matches();

    if !matches.is_present("run") && !matches.is_present("build") {
        println!("Supply either '-b' (build) or '-r' (run)");
        return;
    }

    for arg in matches.values_of("INPUT").unwrap().into_iter() {
        //println!("arg: {}", arg);

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

        if matches.is_present("run") {
            eval::eval_engine(&bc, "main", &mut None);
        }

        if matches.is_present("build") {
            let compile_result = codegen::compile_bytecode(&bc, &arg);
            println!("\nCompile result: {:?}", compile_result);
        }
    }
}
