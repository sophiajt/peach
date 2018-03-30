#![feature(match_default_bindings, nll)]
extern crate syn;
use syn::{BinOp, Expr, Item, ItemFn, Lit, Pat, ReturnType, Stmt, Type};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

extern crate time;
use time::PreciseTime;

type VarId = usize;

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
    VarDecl(VarId),
    Ident(VarId),
}

#[derive(Debug, PartialEq, Clone)]
enum Ty {
    U64,
    Bool,
    Error,
    Void,
}

#[derive(Debug, Clone)]
enum EvalValue {
    U64(u64),
    Bool(bool),
    Error,
    Void,
}

struct VarDecl {
    ident: String,
    ty: Ty,
}

impl VarDecl {
    fn new(ident: String, ty: Ty) -> VarDecl {
        VarDecl { ident, ty }
    }
}

struct Context {
    scope: Vec<usize>,
    vars: Vec<VarDecl>,
}

impl Context {
    fn new() -> Context {
        Context {
            scope: vec![],
            vars: vec![],
        }
    }

    fn add_var(&mut self, ident: String, ty: Ty) -> usize {
        self.vars.push(VarDecl::new(ident, ty));
        let pos = self.vars.len() - 1;
        self.scope.push(pos);
        pos
    }

    fn find_var(&self, ident: &String) -> usize {
        for var in &self.scope {
            if ident == &self.vars[*var].ident {
                return *var;
            }
        }
        unimplemented!("Could not find variable: {}", ident);
    }
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
    ctxt: &mut Context,
) -> Ty {
    match expr {
        Expr::Return(er) => {
            let actual_return_type = match er.expr {
                Some(ref inner) => {
                    convert_expr_to_bytecode(inner, expected_return_type, bytecode, ctxt)
                }
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
                let lhs_type =
                    convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode, ctxt);
                let rhs_type =
                    convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode, ctxt);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Add);
                    Ty::U64
                } else {
                    unimplemented!("Can't add values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            BinOp::Sub(_a) => {
                let lhs_type =
                    convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode, ctxt);
                let rhs_type =
                    convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode, ctxt);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Sub);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            BinOp::Mul(_a) => {
                let lhs_type =
                    convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode, ctxt);
                let rhs_type =
                    convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode, ctxt);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Mul);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            BinOp::Div(_a) => {
                let lhs_type =
                    convert_expr_to_bytecode(&*eb.left, expected_return_type, bytecode, ctxt);
                let rhs_type =
                    convert_expr_to_bytecode(&*eb.right, expected_return_type, bytecode, ctxt);
                if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                    bytecode.push(Bytecode::Div);
                    Ty::U64
                } else {
                    unimplemented!("Can't subtract values of {:?} and {:?}", lhs_type, rhs_type);
                }
            }
            _ => unimplemented!("Unknown operator: {:?}", eb.op),
        },
        Expr::Path(ep) => {
            let ident = ep.path.segments[0].ident.to_string();

            let var_id = ctxt.find_var(&ident);
            let var = &ctxt.vars[var_id];

            bytecode.push(Bytecode::Ident(var_id));

            var.ty.clone()
        }
        _ => unimplemented!("Unknown expr type: {:#?}", expr),
    }
}

fn convert_stmt_to_bytecode(
    stmt: &Stmt,
    expected_return_type: &Ty,
    bytecode: &mut Vec<Bytecode>,
    ctxt: &mut Context,
) {
    match stmt {
        Stmt::Semi(ref e, _) => {
            convert_expr_to_bytecode(e, expected_return_type, bytecode, ctxt);
        }
        Stmt::Expr(ref e) => {
            // TODO: refactor the two styles of return?
            let actual_return_type =
                convert_expr_to_bytecode(e, expected_return_type, bytecode, ctxt);

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
        Stmt::Local(ref l) => {
            match l.init {
                Some(ref foo) => {
                    let ty =
                        convert_expr_to_bytecode(&*foo.1, expected_return_type, bytecode, ctxt);

                    //TODO check this type against the given type
                    match l.ty {
                        None => {
                            let ident = match *l.pat {
                                Pat::Ident(ref pi) => pi.ident.to_string(),
                                _ => unimplemented!("Unsupport pattern in variable declaration"),
                            };

                            let var_id = ctxt.add_var(ident, ty);
                            bytecode.push(Bytecode::VarDecl(var_id));
                        }
                        Some(ref _ty) => unimplemented!("Can't understand explicit var decl type"),
                    }
                }
                None => unimplemented!("Can't yet handle inferred types or uninit variables"),
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

    let mut ctxt = Context::new();

    for stmt in &fun.block.stmts {
        convert_stmt_to_bytecode(stmt, &return_type, &mut output, &mut ctxt);
    }

    (return_type, output)
}

fn eval_bytecode(bytecode: &Vec<Bytecode>) -> EvalValue {
    let mut value_stack: Vec<EvalValue> = vec![];
    let mut var_lookup: HashMap<usize, usize> = HashMap::new();

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
            Bytecode::VarDecl(var_id) => {
                var_lookup.insert(*var_id, value_stack.len() - 1);
            }
            Bytecode::Ident(var_id) => {
                let pos: usize = var_lookup[var_id];
                value_stack.push(value_stack[pos].clone());
            }
        }
    }

    EvalValue::Void
}

fn codegen_bytecode(fn_name: &str, return_type: &Ty, bytecode: &Vec<Bytecode>, output_fn: &str) {
    let mut output = String::new();

    let mut var_lookup: HashMap<usize, usize> = HashMap::new();

    output += "#include <stdio.h>\n";
    output += "#include <stdbool.h>\n";

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
    //TODO: do we need the types here if we can just use 'auto' when we're not sure?
    let mut var_name_stack: Vec<(usize, Ty)> = vec![];

    for bc in bytecode {
        match bc {
            Bytecode::ReturnVoid => {
                output += "return;\n";
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let (retval, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                output += &format!("return v{};\n", retval);
                break;
            }
            Bytecode::PushU64(val) => {
                output += &format!("unsigned long long v{} = {};\n", next_id, val);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::PushBool(val) => {
                output += &format!("bool v{} = {};\n", next_id, val);
                var_name_stack.push((next_id, Ty::Bool));
                next_id += 1;
            }
            Bytecode::Add => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}+v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Sub => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}-v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Mul => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}*v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Div => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}/v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::VarDecl(var_id) => {
                let last_pos = var_name_stack.len() - 1;
                var_lookup.insert(*var_id, last_pos);
            }
            Bytecode::Ident(var_id) => {
                let id = var_lookup[var_id];
                // TODO: we need a better way to output the type name
                let (ref var, ref ty) = var_name_stack[id];

                match ty {
                    Ty::Bool => {
                        output += &format!("bool v{} = v{};\n", next_id, var);
                    }
                    Ty::U64 => {
                        output += &format!("unsigned long long v{} = v{};\n", next_id, var);
                    }
                    _ => unimplemented!("Can't reference a variable of type {:?};\n", ty),
                };

                var_name_stack.push((next_id, ty.clone()));
                next_id += 1;
            }
        }
    }

    output += "};\n";

    output += "int main() { printf(\"output: %llu\", expr()); }";

    let path = {
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::Path;

        let dir = std::env::temp_dir();
        let path = Path::new(&dir).join("madness.c");
        let mut file =
            File::create(path.clone()).expect("Can not create temporary .c file for output");
        file.write_all(&output.as_bytes())
            .expect("Failed to write output to .c file");
        path
    };

    {
        let start = PreciseTime::now();
        println!("path: {:?}", path);
        use std::process::Command;
        let output = Command::new(r"C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Tools\MSVC\14.13.26128\bin\Hostx64\x64\cl.exe")
            //.arg("/Ox")
            .arg(path)
            .output()
            .expect("failed to execute compiler");
        let end = PreciseTime::now();
        let duration = start
            .to(end)
            .to_std()
            .expect("Can't convert duration to std duration");

        println!(
            "status: {} in {:.3} sec",
            output.status,
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        );
        println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    }
}

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
                    let (return_type, bytecode) = convert_fn_to_bytecode(item_fn);
                    println!("{:?}", bytecode);
                    println!("eval: {:?}", eval_bytecode(&bytecode));
                    codegen_bytecode(&fn_name, &return_type, &bytecode, "local.exe");
                }
                _ => {
                    unimplemented!("Unknown item type");
                }
            }
        }
    }
}
