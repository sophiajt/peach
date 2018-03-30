#![feature(match_default_bindings, nll)]
extern crate syn;
extern crate time;

use std::env;
use std::fs::File;
use std::io::Read;

mod bytecode;
mod codegen;
mod eval;

fn main() {
    let mut args = env::args();
    let _ = args.next(); // executable name

    for arg in args {
        let mut file = File::open(arg).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");

        let mut bc = bytecode::convert_file_to_bytecode(syntax_file);
        let fn_bytecode = bc.get_fn_bytecode(&"expr".into());

        println!("{:?}", fn_bytecode.bytecode);
        println!("eval: {:?}", eval::eval_bytecode(&fn_bytecode.bytecode));
        codegen::codegen_bytecode("expr", &fn_bytecode.return_ty, &fn_bytecode.bytecode);
    }
}
