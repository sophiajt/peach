#![feature(match_default_bindings)]
extern crate syn;
use syn::{Block, Expr, FnDecl, Item, Lit, Stmt};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

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
                println!("Got: {:?}", li.value());
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

fn codegen_expr(expr: &Expr) -> String {
    let mut output = String::new();

    match expr {
        Expr::Lit(el) => match el.lit {
            Lit::Int(ref li) => {
                output += &li.value().to_string();
            }
            _ => unimplemented!("unknown expr type"),
        },
        _ => unimplemented!("unknown expr type"),
    }

    output
}

fn codegen_stmt(stmt: &Stmt) -> String {
    let mut output = String::new();
    match stmt {
        Stmt::Semi(ref er, _) => match er {
            Expr::Return(ref r) => match r.expr {
                Some(ref inner) => {
                    output += "return ";
                    output += &codegen_expr(inner);
                    output += ";\n";
                }
                _ => unimplemented!("expected return value"),
            },
            _ => unimplemented!("unknown semi type"),
        },
        _ => unimplemented!("unknown stmt type"),
    }

    output
}

fn codegen_fun(fun: &Fun) -> String {
    let mut output = String::new();

    output += "void foo() {\n";

    for stmt in &fun.block.stmts {
        output += &codegen_stmt(stmt);
    }

    output += "}";

    output
}

fn main() {
    let mut fn_hash = HashMap::new();

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
        println!("codegen: {}", codegen_fun(&fn_hash["main"]));
    }
}
