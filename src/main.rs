#![feature(match_default_bindings)]
extern crate syn;
use syn::{BinOp, Block, Expr, FnDecl, Item, Lit, Stmt};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

extern crate time;
use time::PreciseTime;

#[derive(Debug)]
struct Fun {
    decl: Box<FnDecl>,
    block: Box<Block>,
}

fn eval_expr(expr: &Expr) -> u64 {
    match expr {
        Expr::Return(er) => match er.expr {
            Some(ref inner) => eval_expr(&inner),
            _ => unimplemented!("expected return value"),
        },
        Expr::Lit(el) => match el.lit {
            Lit::Int(ref li) => li.value(),
            _ => unimplemented!("unknown literal"),
        },
        Expr::Binary(eb) => match eb.op {
            BinOp::Add(_a) => {
                let lhs = eval_expr(&*eb.left);
                let rhs = eval_expr(&*eb.right);
                lhs + rhs
            }
            BinOp::Sub(_s) => {
                let lhs = eval_expr(&*eb.left);
                let rhs = eval_expr(&*eb.right);
                lhs - rhs
            }
            BinOp::Mul(_m) => {
                let lhs = eval_expr(&*eb.left);
                let rhs = eval_expr(&*eb.right);
                lhs * rhs
            }
            BinOp::Div(_d) => {
                let lhs = eval_expr(&*eb.left);
                let rhs = eval_expr(&*eb.right);
                lhs / rhs
            }
            _ => unimplemented!("unknown operator"),
        },
        _ => unimplemented!("unknown expr type"),
    }
}

fn eval_stmt(stmt: &Stmt) -> u64 {
    match stmt {
        Stmt::Semi(ref e, _) => eval_expr(e),
        Stmt::Expr(ref e) => eval_expr(e),
        _ => unimplemented!("unknown stmt type: {:#?}", stmt),
    }
}

fn eval_fun(fun: &Fun) -> u64 {
    let mut total = 0;
    for stmt in &fun.block.stmts {
        total += eval_stmt(stmt);
    }
    total
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
        Expr::Binary(eb) => match eb.op {
            BinOp::Add(_a) => {
                let lhs = codegen_expr(&*eb.left);
                let rhs = codegen_expr(&*eb.right);
                output += "(";
                output += &lhs;
                output += ")+(";
                output += &rhs;
                output += ")";
            }
            BinOp::Sub(_s) => {
                let lhs = codegen_expr(&*eb.left);
                let rhs = codegen_expr(&*eb.right);
                output += "(";
                output += &lhs;
                output += ")-(";
                output += &rhs;
                output += ")";
            }
            BinOp::Mul(_m) => {
                let lhs = codegen_expr(&*eb.left);
                let rhs = codegen_expr(&*eb.right);
                output += "(";
                output += &lhs;
                output += ")*(";
                output += &rhs;
                output += ")";
            }
            BinOp::Div(_d) => {
                let lhs = codegen_expr(&*eb.left);
                let rhs = codegen_expr(&*eb.right);
                output += "(";
                output += &lhs;
                output += ")/(";
                output += &rhs;
                output += ")";
            }
            _ => unimplemented!("unknown operator"),
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
            _ => {
                output += &codegen_expr(er);
                output += ";\n";
            }
        },
        Stmt::Expr(ref e) => {
            output += "return ";
            output += &codegen_expr(e);
            output += ";\n";
        }
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
        let start = PreciseTime::now();
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
        let end = PreciseTime::now();
        println!("processing time: {}", start.to(end));
        let start = PreciseTime::now();
        //println!("{:#?}", fn_hash);
        if eval_fun(&fn_hash["main"]) == 0 {
            println!("Main failed to produce a value != 0");
        }
        let end = PreciseTime::now();
        println!("eval time (total): {}", start.to(end));
        //println!("codegen: {}", codegen_fun(&fn_hash["main"]));

        let start = PreciseTime::now();
        let length = codegen_fun(&fn_hash["main"]).len();
        let end = PreciseTime::now();
        println!("codegen length: {}", length);
        println!("compile time (total): {}", start.to(end));
    }
}
