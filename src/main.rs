#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

use std::env;
use std::fs::File;
use std::io::Read;

mod bytecode;
mod codegen;
mod eval;

use bytecode::BytecodeConverter;

fn main() {
    let mut args = env::args();
    let _ = args.next(); // executable name

    for arg in args {
        let mut file = File::open(arg).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");

        let mut bc = BytecodeConverter::new();

        // Step 1: Load up the parsed file so that we can lazily convert it
        bc.load_file(syntax_file);

        // Step 2: Tell the converter where to start, and we'll get the output
        let fn_bytecode = bc.convert_fn("expr");

        println!("{:?}", fn_bytecode.bytecode);
        println!("eval: {:?}", eval::eval_bytecode(&fn_bytecode.bytecode));
        codegen::codegen_bytecode("expr", &fn_bytecode.return_ty, &fn_bytecode.bytecode);
    }
}
