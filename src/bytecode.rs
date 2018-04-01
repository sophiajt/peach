use std::collections::HashMap;
use syn::{self, BinOp, Block, Expr, FnArg, Item, ItemFn, Lit, Pat, ReturnType, Stmt, Type};

type VarId = usize;

#[derive(Debug, Clone)]
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
    Assign(VarId),
    Call(String),
    If(usize), // usize here is the offset ahead to jump if the condition is not matched
    DebugPrint,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    U64,
    Bool,
    Error,
    Void,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub var_id: VarId,
    pub ty: Ty,
}
impl Param {
    fn new(name: String, var_id: VarId, ty: Ty) -> Param {
        Param { name, var_id, ty }
    }
}

//TODO: should VarDecl and Param be merged?
#[derive(Clone)]
struct VarDecl {
    ident: String,
    ty: Ty,
}

impl VarDecl {
    fn new(ident: String, ty: Ty) -> VarDecl {
        VarDecl { ident, ty }
    }
}

#[derive(Clone)]
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

#[derive(Debug, Clone)]
pub struct Fun {
    pub params: Vec<Param>,
    pub return_ty: Ty,
    pub bytecode: Vec<Bytecode>,
}

pub struct BytecodeEngine {
    lazy_fns: HashMap<String, ItemFn>,

    //TODO probably don't want this to be public
    pub processed_fns: HashMap<String, Fun>,
}

impl BytecodeEngine {
    pub fn new() -> BytecodeEngine {
        BytecodeEngine {
            lazy_fns: HashMap::new(),
            processed_fns: HashMap::new(),
        }
    }

    // Gets the bytecoded function for the given name
    pub fn get_fn(&self, fn_name: &str) -> &Fun {
        if !self.processed_fns.contains_key(fn_name) {
            unimplemented!("Function {} needs to be precomputed", fn_name);
        }
        &self.processed_fns[fn_name]
    }

    pub fn load_file(&mut self, syntax_file: syn::File) {
        for item in syntax_file.items {
            match item {
                Item::Fn(item_fn) => {
                    let fn_name = item_fn.ident.to_string();
                    self.add_lazy(fn_name, item_fn);
                }
                _ => {
                    unimplemented!("Unknown item type: {:#?}", item);
                }
            }
        }
    }

    pub fn process(&mut self, starting_fn_name: &str) {
        let fun = self.convert_fn_to_bytecode(starting_fn_name);
        self.processed_fns.insert(starting_fn_name.to_string(), fun);
    }

    fn add_lazy(&mut self, fn_name: String, item_fn: ItemFn) {
        self.lazy_fns.insert(fn_name, item_fn);
    }

    fn resolve_type(&mut self, tp: &Type) -> Ty {
        match *tp {
            Type::Path(ref tp) => match tp.path.segments[0].ident.as_ref() {
                "u64" => Ty::U64,
                "bool" => Ty::Bool,
                _ => Ty::Error,
            },
            _ => Ty::Error,
        }
    }

    fn convert_expr_to_bytecode(
        &mut self,
        expr: &Expr,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        ctxt: &mut Context,
    ) -> Ty {
        match expr {
            Expr::Return(er) => {
                let actual_return_type = match er.expr {
                    Some(ref inner) => {
                        self.convert_expr_to_bytecode(inner, expected_return_type, bytecode, ctxt)
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
            Expr::Paren(ep) => {
                self.convert_expr_to_bytecode(&*ep.expr, expected_return_type, bytecode, ctxt)
            }
            Expr::Assign(ea) => {
                let rhs_type =
                    self.convert_expr_to_bytecode(&*ea.right, expected_return_type, bytecode, ctxt);

                match &*ea.left {
                    Expr::Path(ep) => {
                        let ident = ep.path.segments[0].ident.to_string();

                        let var_id = ctxt.find_var(&ident);
                        let var = &ctxt.vars[var_id];

                        if rhs_type != var.ty {
                            unimplemented!("Assignment between incompatible types");
                        }

                        bytecode.push(Bytecode::Assign(var_id));
                    }
                    _ => unimplemented!("Unsupported variable path for assignment"),
                }

                Ty::Void
            }
            Expr::If(ei) => {
                let cond_type =
                    self.convert_expr_to_bytecode(&*ei.cond, expected_return_type, bytecode, ctxt);

                match cond_type {
                    Ty::Bool => {}
                    _ => unimplemented!("If condition needs to be boolean"),
                }

                bytecode.push(Bytecode::If(0));
                let before_then_len = bytecode.len();

                let then_ty = self.convert_block_to_bytecode(
                    &ei.then_branch,
                    expected_return_type,
                    bytecode,
                    ctxt,
                );

                let after_then_len = bytecode.len();

                // Patch the original offset to the correct offset
                bytecode[before_then_len - 1] = Bytecode::If(after_then_len - before_then_len + 1);

                then_ty
            }
            Expr::Binary(eb) => match eb.op {
                BinOp::Add(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                        bytecode.push(Bytecode::Add);
                        Ty::U64
                    } else {
                        unimplemented!("Can't add values of {:?} and {:?}", lhs_type, rhs_type);
                    }
                }
                BinOp::Sub(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                        bytecode.push(Bytecode::Sub);
                        Ty::U64
                    } else {
                        unimplemented!(
                            "Can't subtract values of {:?} and {:?}",
                            lhs_type,
                            rhs_type
                        );
                    }
                }
                BinOp::Mul(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                        bytecode.push(Bytecode::Mul);
                        Ty::U64
                    } else {
                        unimplemented!(
                            "Can't subtract values of {:?} and {:?}",
                            lhs_type,
                            rhs_type
                        );
                    }
                }
                BinOp::Div(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );
                    if lhs_type == Ty::U64 && rhs_type == Ty::U64 {
                        bytecode.push(Bytecode::Div);
                        Ty::U64
                    } else {
                        unimplemented!(
                            "Can't subtract values of {:?} and {:?}",
                            lhs_type,
                            rhs_type
                        );
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
            Expr::Call(ec) => match *ec.func {
                Expr::Path(ref ep) => {
                    let ident = ep.path.segments[0].ident.to_string();
                    if ident == "__debug__" {
                        self.convert_expr_to_bytecode(
                            &ec.args[0],
                            expected_return_type,
                            bytecode,
                            ctxt,
                        );
                        bytecode.push(Bytecode::DebugPrint);
                        Ty::Void
                    } else {
                        self.process(&ident);

                        let target_fn = self.get_fn(&ident);
                        let return_ty = target_fn.return_ty.clone();

                        for arg in &ec.args {
                            self.convert_expr_to_bytecode(
                                arg,
                                expected_return_type,
                                bytecode,
                                ctxt,
                            );
                        }

                        bytecode.push(Bytecode::Call(ident));

                        return_ty
                    }
                }
                _ => unimplemented!("unknown function call type: {:#?}", ec.func),
            },
            _ => unimplemented!("Unknown expr type: {:#?}", expr),
        }
    }

    fn convert_stmt_to_bytecode(
        &mut self,
        stmt: &Stmt,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        ctxt: &mut Context,
    ) -> Ty {
        match stmt {
            Stmt::Semi(ref e, _) => {
                self.convert_expr_to_bytecode(e, expected_return_type, bytecode, ctxt);
                Ty::Void
            }
            Stmt::Expr(ref e) => {
                // TODO: refactor the two styles of return?
                let expr_type =
                    self.convert_expr_to_bytecode(e, expected_return_type, bytecode, ctxt);

                expr_type
            }
            Stmt::Local(ref l) => {
                match l.init {
                    Some(ref foo) => {
                        let ty = self.convert_expr_to_bytecode(
                            &*foo.1,
                            expected_return_type,
                            bytecode,
                            ctxt,
                        );

                        //TODO check this type against the given type
                        match l.ty {
                            None => {
                                let ident = match *l.pat {
                                    Pat::Ident(ref pi) => pi.ident.to_string(),
                                    _ => {
                                        unimplemented!("Unsupport pattern in variable declaration")
                                    }
                                };

                                let var_id = ctxt.add_var(ident, ty);
                                bytecode.push(Bytecode::VarDecl(var_id));
                                Ty::Void
                            }
                            Some(ref _ty) => {
                                unimplemented!("Can't understand explicit var decl type")
                            }
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

    fn convert_block_to_bytecode(
        &mut self,
        block: &Block,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        ctxt: &Context,
    ) -> Ty {
        //TODO: there may be more efficient ways to do this, but this will do for now
        let mut block_ctxt = ctxt.clone();
        let mut return_ty = Ty::Void;

        for stmt in &block.stmts {
            return_ty = self.convert_stmt_to_bytecode(
                stmt,
                &expected_return_type,
                bytecode,
                &mut block_ctxt,
            );
        }

        return_ty
    }

    fn convert_fn_to_bytecode(&mut self, fn_name: &str) -> Fun {
        if self.processed_fns.contains_key(fn_name) {
            let result = &self.processed_fns[fn_name];
            result.clone()
        } else {
            let item_fn = self.lazy_fns[fn_name].clone();

            let mut bytecode = Vec::new();

            let return_ty = match &item_fn.decl.output {
                ReturnType::Default => Ty::Void,
                ReturnType::Type(_, ref box_ty) => self.resolve_type(box_ty),
            };

            let mut ctxt = Context::new();
            let mut params = vec![];

            // process function params
            for input in &item_fn.decl.inputs {
                match input {
                    FnArg::Captured(ref capture) => {
                        match capture.pat {
                            Pat::Ident(ref pi) => {
                                let ident = pi.ident.to_string();
                                let ty = self.resolve_type(&capture.ty);
                                let var_id = ctxt.add_var(ident.clone(), ty.clone());
                                params.push(Param::new(ident, var_id, ty));
                            }
                            _ => unimplemented!("Unsupported pattern type in function parameter"),
                        };
                    }
                    _ => unimplemented!("Function argument of {:?} is not supported", input),
                }
            }

            let block_ty =
                self.convert_block_to_bytecode(&item_fn.block, &return_ty, &mut bytecode, &ctxt);

            match block_ty {
                Ty::Void => bytecode.push(Bytecode::ReturnVoid),
                _ => bytecode.push(Bytecode::ReturnLastStackValue),
            }

            if block_ty != return_ty {
                unimplemented!(
                    "Mismatched return types: {:?} and {:?}",
                    block_ty,
                    return_ty
                );
            }

            Fun {
                params,
                return_ty,
                bytecode,
            }
        }
    }
}
