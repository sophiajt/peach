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
    PushBool(bool),
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
enum Ty {
    U64,
    Bool,
    Error,
    Void,
}

#[derive(Debug)]
enum EvalValue {
    U64(u64),
    Bool(bool),
    Error,
    Void,
}

fn resolve_type(tp: &Box<Type>) -> Ty {
    match **tp {
        Type::Path(ref tp) => match tp.path.segments[0].ident.as_ref() {
            "u64" => Ty::U64,
            "bool" => Ty::Bool,
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
            Lit::Bool(ref lb) => {
                bytecode.push(Bytecode::PushBool(lb.value));
                Ty::Bool
            }
            _ => unimplemented!("unknown literal: {:?}", el),
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
            BinOp::Sub(_a) => {
                let lhs_type = convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode);
                let rhs_type = convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Sub);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            BinOp::Mul(_a) => {
                let lhs_type = convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode);
                let rhs_type = convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Mul);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            BinOp::Div(_a) => {
                let lhs_type = convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode);
                let rhs_type = convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Div);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            _ => unimplemented!("Unknown operator: {:?}", eb.op),
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

fn convert_fn_to_bytecode(fun: ItemFn) -> (Ty, Vec<Bytecode>) {
    let mut output = Vec::new();

    let return_type = match &fun.decl.output {
        ReturnType::Default => Ty::Void,
        ReturnType::Type(_, ref box_ty) => resolve_type(box_ty),
    };

    for stmt in &fun.block.stmts {
        convert_stmt_to_bytecode(stmt, &return_type, &mut output);
    }

    (return_type, output)
}

fn eval_bytecode(bytecode: &Vec<Bytecode>) -> EvalValue {
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
                (Some(EvalValue::U64(rhs)), Some(EvalValue::U64(lhs))) => {
                    value_stack.push(EvalValue::U64(lhs + rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Sub => match (value_stack.pop(), value_stack.pop()) {
                (Some(EvalValue::U64(rhs)), Some(EvalValue::U64(lhs))) => {
                    value_stack.push(EvalValue::U64(lhs - rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Mul => match (value_stack.pop(), value_stack.pop()) {
                (Some(EvalValue::U64(rhs)), Some(EvalValue::U64(lhs))) => {
                    value_stack.push(EvalValue::U64(lhs * rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Div => match (value_stack.pop(), value_stack.pop()) {
                (Some(EvalValue::U64(rhs)), Some(EvalValue::U64(lhs))) => {
                    value_stack.push(EvalValue::U64(lhs / rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::PushU64(val) => {
                value_stack.push(EvalValue::U64(*val));
            }
            Bytecode::PushBool(val) => {
                value_stack.push(EvalValue::Bool(*val));
            }
        }
    }

    EvalValue::Void
}

fn codegen_bytecode(fn_name: &str, return_type: &Ty, bytecode: &Vec<Bytecode>) -> String {
    let mut output = String::new();

    match return_type {
        Ty::U64 => {
            output += "unsigned long long ";
        }
        Ty::Void => {
            output += "void ";
        }
        Ty::Bool => {
            output += "bool ";
        }
        _ => unimplemented!("Can't codegen function with return type: {:?}", return_type),
    }

    output += fn_name;
    output += "() {\n";

    let mut next_id = 0;
    let mut var_name_stack = vec![];
    for bc in bytecode {
        match bc {
            Bytecode::ReturnVoid => {
                output += "return;\n";
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let retval = var_name_stack.pop().expect("Add needs a rhs in codegen");
                output += &format!("return v{};\n", retval);
                break;
            }
            Bytecode::PushU64(val) => {
                output += &format!("unsigned long long v{} = {};\n", next_id, val);
                var_name_stack.push(next_id);
                next_id += 1;
            }
            Bytecode::PushBool(val) => {
                output += &format!("bool v{} = {};\n", next_id, val);
                var_name_stack.push(next_id);
                next_id += 1;
            }
            Bytecode::Add => {
                let rhs = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let lhs = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}+v{};\n", next_id, lhs, rhs);
                var_name_stack.push(next_id);
                next_id += 1;
            }
            Bytecode::Sub => {
                let rhs = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let lhs = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}-v{};\n", next_id, lhs, rhs);
                var_name_stack.push(next_id);
                next_id += 1;
            }
            Bytecode::Mul => {
                let rhs = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let lhs = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}*v{};\n", next_id, lhs, rhs);
                var_name_stack.push(next_id);
                next_id += 1;
            }
            Bytecode::Div => {
                let rhs = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let lhs = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}/v{};\n", next_id, lhs, rhs);
                var_name_stack.push(next_id);
                next_id += 1;
            }
        }
    }

    output += "}";

    output
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
                    let fn_name = item_fn.ident.to_string();
                    let (return_type, bytecode) = convert_fn_to_bytecode(item_fn);
                    println!("{:?}", bytecode);
                    println!("eval: {:?}", eval_bytecode(&bytecode));
                    println!(
                        "codegen: {}",
                        codegen_bytecode(&fn_name, &return_type, &bytecode)
                    );
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
