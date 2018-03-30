#![feature(match_default_bindings, nll)]
extern crate syn;
use syn::Item;

use std::env;
use std::fs::File;
use std::io::Read;

extern crate time;

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

        let syntax = syn::parse_file(&src).expect("Unable to parse file");

        for item in syntax.items {
            match item {
                Item::Fn(item_fn) => {
                    let fn_name = item_fn.ident.to_string();
                    let (return_type, bytecode) = bytecode::convert_fn_to_bytecode(item_fn);
                    println!("{:?}", bytecode);
                    println!("eval: {:?}", eval::eval_bytecode(&bytecode));
                    codegen::codegen_bytecode(&fn_name, &return_type, &bytecode);
                }
                _ => {
                    unimplemented!("Unknown item type: {:#?}", item);
                }
            }
        }
    }
}
