#![feature(match_default_bindings)]
extern crate syn;
use syn::{Block, Expr, FnDecl, Item, Lit, Stmt};

use std::env;
use std::fs::File;
use std::io::Read;
use std::process;
use std::collections::HashMap;

#[derive(Debug)]
struct Fun {
    decl: Box<FnDecl>,
    block: Box<Block>,
}

fn eval_expr(expr: &Expr) {
    match expr {
        Expr::Return(er) => match er.expr {
            Some(ref inner) => {
                eval_expr(&inner);
            }
            _ => unimplemented!("expected return value"),
        },
        Expr::Lit(el) => match el.lit {
            Lit::Int(ref li) => {
                println!("Got: {:?}", li);
            }
            _ => unimplemented!("unknown literal"),
        },
        _ => unimplemented!("unknown expr type"),
    }
}

fn eval_stmt(stmt: &Stmt) {
    match stmt {
        Stmt::Semi(ref e, _) => {
            eval_expr(e);
        }
        _ => unimplemented!("unknown stmt type"),
    }
}

fn eval_fun(fun: &Fun) {
    for stmt in &fun.block.stmts {
        eval_stmt(stmt);
    }
}

fn main() {
    let mut fn_hash = HashMap::new();

    let mut args = env::args();
    let _ = args.next(); // executable name

    let filename = match (args.next(), args.next()) {
        (Some(filename), None) => filename,
        _ => {
            eprintln!("Usage: dump-syntax path/to/filename.rs");
            process::exit(1);
        }
    };

    let mut file = File::open(&filename).expect("Unable to open file");

    let mut src = String::new();
    file.read_to_string(&mut src).expect("Unable to read file");

    let syntax = syn::parse_file(&src).expect("Unable to parse file");

    for item in syntax.items {
        match item {
            Item::Fn(item_fn) => {
                let fun = Fun {
                    decl: item_fn.decl,
                    block: item_fn.block,
                };

                let fn_name = format!("{}", item_fn.ident);

                fn_hash.insert(fn_name, fun);
            }
            _ => {
                unimplemented!("Unknown item type");
            }
        }
    }
    println!("{:#?}", fn_hash);
    eval_fun(&fn_hash["main"]);
}
