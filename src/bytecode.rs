use std::collections::HashMap;
use syn::{self, BinOp, Expr, Item, ItemFn, Lit, Pat, ReturnType, Stmt, Type};

type VarId = usize;

#[derive(Debug)]
pub enum Bytecode {
    ReturnLastStackValue,
    ReturnVoid,
    PushU64(u64),
    PushBool(bool),
    Add,
    Sub,
    Mul,
    Div,
    VarDecl(VarId),
    Var(VarId),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    U64,
    Bool,
    Error,
    Void,
}

#[derive(Debug)]
pub struct FnBytecode {
    pub return_ty: Ty,
    pub bytecode: Vec<Bytecode>,
}

pub struct BytecodeContext {
    lazy_fns: HashMap<String, ItemFn>,
    converted_fns: HashMap<String, FnBytecode>,
}

impl BytecodeContext {
    fn new() -> BytecodeContext {
        BytecodeContext {
            lazy_fns: HashMap::new(),
            converted_fns: HashMap::new(),
        }
    }

    fn add_lazy(&mut self, fn_name: String, item_fn: ItemFn) {
        self.lazy_fns.insert(fn_name, item_fn);
    }

    pub fn get_fn_bytecode(&mut self, fn_name: &String) -> &FnBytecode {
        if !self.converted_fns.contains_key(fn_name) {
            let item_fn = &self.lazy_fns[fn_name];
            let (return_ty, bytecode) = convert_fn_to_bytecode(item_fn);
            self.converted_fns.insert(
                fn_name.clone(),
                FnBytecode {
                    return_ty,
                    bytecode,
                },
            );
        }
        &self.converted_fns[fn_name]
    }
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

            bytecode.push(Bytecode::Var(var_id));

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
        Stmt::Item(ref i) => match i {
            _ => unimplemented!("Unknown item type: {:?}", i),
        },
    }
}

fn convert_fn_to_bytecode(fun: &ItemFn) -> (Ty, Vec<Bytecode>) {
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

//TODO - this is not how we ultimately want to return the bytecode, but it will do for now
pub fn convert_file_to_bytecode(syntax_file: syn::File) -> BytecodeContext {
    let mut bc = BytecodeContext::new();

    for item in syntax_file.items {
        match item {
            Item::Fn(item_fn) => {
                let fn_name = item_fn.ident.to_string();
                bc.add_lazy(fn_name, item_fn);
            }
            _ => {
                unimplemented!("Unknown item type: {:#?}", item);
            }
        }
    }

    bc
}
