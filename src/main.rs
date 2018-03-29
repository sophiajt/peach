#![feature(match_default_bindings)]
extern crate syn;
use syn::{BinOp, Block, Expr, FnDecl, Item, ItemFn, Lit, Path, ReturnType, Stmt, Type, TypePath};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

extern crate time;
//use time::PreciseTime;

#[derive(Debug)]
enum Bytecode {
    ReturnLastStackValue,
    ReturnVoid,
    PushU64(u64),
    Add,
}

#[derive(Debug, PartialEq)]
enum Ty {
    U64,
    Error,
    Void,
}

#[derive(Debug)]
enum EvalValue {
    U64(u64),
    Error,
    Void,
}

fn resolve_type(tp: &Box<Type>) -> Ty {
    match **tp {
        Type::Path(ref tp) => match tp.path.segments[0].ident.as_ref() {
            "u64" => Ty::U64,
            _ => Ty::Error,
        },
        _ => Ty::Error,
    }
}

fn convert_expr_to_bytecode(
    expr: &Expr,
    expected_return_type: &Ty,
    bytecode: &mut Vec<Bytecode>,
) -> Ty {
    match expr {
        Expr::Return(er) => {
            let actual_return_type = match er.expr {
                Some(ref inner) => convert_expr_to_bytecode(inner, expected_return_type, bytecode),
                None => Ty::Void,
            };

            if actual_return_type == *expected_return_type {
                match actual_return_type {
                    Ty::Void => bytecode.push(Bytecode::ReturnVoid),
                    _ => bytecode.push(Bytecode::ReturnLastStackValue),
                }
                Ty::Void
            } else {
                println!(
                    "Mismatched return types: {:?} and {:?}",
                    actual_return_type, expected_return_type
                );
                Ty::Error
            }
        }
        Expr::Lit(el) => match el.lit {
            Lit::Int(ref li) => {
                bytecode.push(Bytecode::PushU64(li.value()));
                Ty::U64
            }
            _ => unimplemented!("unknown literal"),
        },
        Expr::Binary(eb) => match eb.op {
            BinOp::Add(_a) => {
                let lhs_type = convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode);
                let rhs_type = convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Add);
                    Ty::U64
                } else {
                    unimplemented!("Can't add values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            _ => unimplemented!("Unknown operator"),
        },
        _ => unimplemented!("Unknown expr type"),
    }
}

fn convert_stmt_to_bytecode(stmt: &Stmt, expected_return_type: &Ty, bytecode: &mut Vec<Bytecode>) {
    match stmt {
        Stmt::Semi(ref e, _) => {
            convert_expr_to_bytecode(e, expected_return_type, bytecode);
        }
        Stmt::Expr(ref e) => {
            // TODO: refactor the two styles of return?
            let actual_return_type = convert_expr_to_bytecode(e, expected_return_type, bytecode);

            if actual_return_type == *expected_return_type {
                match actual_return_type {
                    Ty::Void => bytecode.push(Bytecode::ReturnVoid),
                    _ => bytecode.push(Bytecode::ReturnLastStackValue),
                }
            } else {
                unimplemented!(
                    "Mismatched return types: {:?} and {:?}",
                    actual_return_type,
                    expected_return_type
                );
            }
        }
        _ => unimplemented!("Unknown stmt type: {:#?}", stmt),
    }
}

fn convert_fn_to_bytecode(fun: ItemFn) -> Vec<Bytecode> {
    let mut output = Vec::new();

    let return_type = match &fun.decl.output {
        ReturnType::Default => Ty::Void,
        ReturnType::Type(_, ref box_ty) => resolve_type(box_ty),
    };

    for stmt in &fun.block.stmts {
        convert_stmt_to_bytecode(stmt, &return_type, &mut output);
    }

    output
}

/*
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
*/

fn eval_bytecode(bytecode: &Vec<Bytecode>) -> EvalValue {
    let mut output = EvalValue::Error;
    let mut value_stack: Vec<EvalValue> = vec![];
    for bc in bytecode {
        match bc {
            Bytecode::ReturnVoid => {
                return EvalValue::Void;
            }
            Bytecode::ReturnLastStackValue => match value_stack.pop() {
                Some(s) => return s,
                _ => return EvalValue::Error,
            },
            Bytecode::Add => match (value_stack.pop(), value_stack.pop()) {
                (Some(EvalValue::U64(lhs)), Some(EvalValue::U64(rhs))) => {
                    value_stack.push(EvalValue::U64(lhs + rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::PushU64(val) => {
                value_stack.push(EvalValue::U64(*val));
            }
        }
    }

    EvalValue::Error
}

fn main() {
    //let mut fn_hash = HashMap::new();

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
                    let bytecode = convert_fn_to_bytecode(item_fn);
                    println!("{:?}", bytecode);
                    println!("eval: {:?}", eval_bytecode(&bytecode));
                }
                _ => {
                    unimplemented!("Unknown item type");
                }
            }
        }

        /*
        //println!("{:#?}", fn_hash);
        if eval_fun(&fn_hash["main"]) == 0 {
            println!("Main failed to produce a value != 0");
        }
        //println!("codegen: {}", codegen_fun(&fn_hash["main"]));

        let length = codegen_fun(&fn_hash["main"]).len();
        println!("codegen length: {}", length);
        */
    }
}
