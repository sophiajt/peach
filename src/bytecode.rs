use std::collections::HashMap;
use syn::{self, BinOp, Block, Expr, FnArg, Item, ItemFn, Lit, Pat, ReturnType, Stmt, Type};

type VarId = usize;
type Offset = usize;

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
    Lt,
    VarDecl(VarId),
    Var(VarId),
    Assign(VarId),
    Call(String),
    If(Offset, Ty), // Offset is number of bytecodes to jump forward if false.  Also includes the type of the result, if this is an expression
    Else(Offset, Ty), // Offset is number of bytecodes to skip (aka jump forward). Also includes the type of the result, if this is an expression
    EndIf(Ty),        //includes the type of the result, if this is an expression
    BeginWhile,
    WhileCond(Offset), // Offset is number of bytecodes to jump forward if false
    EndWhile(Offset),  // Offset is number of bytecodes to jump backward to return to start of while
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

    pub fn load_file(&mut self, src: &String) {
        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");

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

                bytecode.push(Bytecode::If(0, Ty::Void));
                let before_then_block_len = bytecode.len();

                let then_ty = self.convert_block_to_bytecode(
                    &ei.then_branch,
                    expected_return_type,
                    bytecode,
                    ctxt,
                );
                let after_then_block_len = bytecode.len();

                if let Some(ref else_branch) = ei.else_branch {
                    bytecode.push(Bytecode::Else(0, Ty::Void));
                    match *else_branch.1 {
                        Expr::Block(ref eb) => {
                            let else_ty = self.convert_block_to_bytecode(
                                &eb.block,
                                expected_return_type,
                                bytecode,
                                ctxt,
                            );

                            if then_ty != else_ty {
                                unimplemented!("If then/else blocks have mismatching types");
                            }
                            bytecode[after_then_block_len] = Bytecode::Else(
                                bytecode.len() - after_then_block_len,
                                else_ty.clone(),
                            );
                        }
                        _ => unimplemented!("Unsupported else block"),
                    }
                }
                bytecode.push(Bytecode::EndIf(then_ty.clone()));

                // Patch the original offset to the correct offset
                bytecode[before_then_block_len - 1] = Bytecode::If(
                    after_then_block_len - before_then_block_len + 2,
                    then_ty.clone(),
                );

                then_ty
            }
            Expr::While(ew) => {
                let before_cond_len = bytecode.len();
                bytecode.push(Bytecode::BeginWhile);

                let cond_type =
                    self.convert_expr_to_bytecode(&*ew.cond, expected_return_type, bytecode, ctxt);

                match cond_type {
                    Ty::Bool => {}
                    _ => unimplemented!("If condition needs to be boolean"),
                }

                bytecode.push(Bytecode::WhileCond(0));
                let before_block_len = bytecode.len();

                let then_ty =
                    self.convert_block_to_bytecode(&ew.body, expected_return_type, bytecode, ctxt);

                let after_block_len = bytecode.len();
                bytecode.push(Bytecode::EndWhile(after_block_len - before_cond_len));

                // Patch the original offset to the correct offset
                bytecode[before_block_len - 1] =
                    Bytecode::WhileCond(after_block_len - before_block_len + 1);

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
                BinOp::Lt(_a) => {
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
                        bytecode.push(Bytecode::Lt);
                        Ty::Bool
                    } else {
                        unimplemented!("Can't compare values of {:?} and {:?}", lhs_type, rhs_type);
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
                self.convert_expr_to_bytecode(e, expected_return_type, bytecode, ctxt)
            }
            Stmt::Local(ref l) => match l.init {
                Some(ref foo) => {
                    let rhs_ty = self.convert_expr_to_bytecode(
                        &*foo.1,
                        expected_return_type,
                        bytecode,
                        ctxt,
                    );

                    match l.ty {
                        None => {
                            let ident = match *l.pat {
                                Pat::Ident(ref pi) => pi.ident.to_string(),
                                _ => unimplemented!("Unsupport pattern in variable declaration"),
                            };

                            let var_id = ctxt.add_var(ident, rhs_ty);
                            bytecode.push(Bytecode::VarDecl(var_id));
                            Ty::Void
                        }
                        Some(ref explicit_ty) => {
                            let var_ty = self.resolve_type(&*explicit_ty.1);

                            let ident = match *l.pat {
                                Pat::Ident(ref pi) => pi.ident.to_string(),
                                _ => unimplemented!("Unsupport pattern in variable declaration"),
                            };

                            if var_ty != rhs_ty {
                                unimplemented!(
                                        "Explicit variable type '{:?}' does not match expression type '{:?}'", var_ty, rhs_ty
                                    )
                            }

                            let var_id = ctxt.add_var(ident, var_ty);
                            bytecode.push(Bytecode::VarDecl(var_id));

                            Ty::Void
                        }
                    }
                }
                None => unimplemented!("Can't yet handle inferred types or uninit variables"),
            },
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
            let item_fn = self.lazy_fns.remove(fn_name).unwrap();

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
