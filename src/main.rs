#![feature(match_default_bindings, nll)]
extern crate syn;
use syn::{BinOp, Block, Expr, FnDecl, Item, ItemFn, Lit, Pat, Path, ReturnType, Stmt, Type,
          TypePath};

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;

extern crate time;
//use time::PreciseTime;

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

fn convert_fn_to_bytecode(fun: ItemFn) -> (Ty, Vec<Bytecode>, Context) {
    let mut output = Vec::new();

    let return_type = match &fun.decl.output {
        ReturnType::Default => Ty::Void,
        ReturnType::Type(_, ref box_ty) => resolve_type(box_ty),
    };

    let mut ctxt = Context::new();

    for stmt in &fun.block.stmts {
        convert_stmt_to_bytecode(stmt, &return_type, &mut output, &mut ctxt);
    }

    (return_type, output, ctxt)
}

fn eval_bytecode(bytecode: &Vec<Bytecode>, ctxt: &Context) -> EvalValue {
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

fn codegen_bytecode(
    fn_name: &str,
    return_type: &Ty,
    bytecode: &Vec<Bytecode>,
    ctxt: &Context,
) -> String {
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
            Bytecode::VarDecl(var_id) => {
                let var = &ctxt.vars[*var_id];
                let rhs = var_name_stack.pop().expect("Add needs a rhs in codegen");

                // TODO: we need a better way to output the type name
                match var.ty {
                    Ty::Bool => {
                        output += &format!("bool {} = v{};\n", &var.ident, rhs);
                    }
                    Ty::U64 => {
                        output += &format!("unsigned long long {} = v{};\n", &var.ident, rhs);
                    }
                    _ => unimplemented!("Can't create a variable of type {:?};\n", var.ty),
                };
            }
            Bytecode::Ident(var_id) => {
                let var = &ctxt.vars[*var_id];
                // TODO: we need a better way to output the type name

                match var.ty {
                    Ty::Bool => {
                        output += &format!("bool v{} = {};\n", next_id, &var.ident);
                    }
                    Ty::U64 => {
                        output += &format!("unsigned long long v{} = {};\n", next_id, var.ident);
                    }
                    _ => unimplemented!("Can't reference a variable of type {:?};\n", var.ty),
                };

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
                    let (return_type, bytecode, ctxt) = convert_fn_to_bytecode(item_fn);
                    println!("{:?}", bytecode);
                    println!("eval: {:?}", eval_bytecode(&bytecode, &ctxt));
                    println!(
                        "codegen: {}",
                        codegen_bytecode(&fn_name, &return_type, &bytecode, &ctxt)
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
