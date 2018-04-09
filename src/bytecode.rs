use std::collections::HashMap;
use std::fmt;

use syn::{self, BinOp, Block, Expr, FnArg, IntSuffix, Item, ItemFn, ItemMod, Lit, Pat, ReturnType,
          Stmt, Type};

type VarId = usize;
type Offset = usize;

#[derive(Debug, Clone)]
pub enum Bytecode {
    ReturnLastStackValue,
    ReturnVoid,
    PushU64(u64),
    PushU32(u32),
    PushBool(bool),
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    VarDecl(VarId),
    VarDeclUninit(VarId),
    Var(VarId),
    Assign(VarId),
    Call(DefinitionId),
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
    U32,
    Bool,
    Error,
    Void,
    Unknown,
    UnknownInt,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ty::U64 => "u64",
                Ty::U32 => "u32",
                Ty::Bool => "bool",
                Ty::Error => "{error}",
                Ty::Void => "void",
                Ty::Unknown => "{unknown}",
                Ty::UnknownInt => "{integer}",
            }
        )
    }
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
#[derive(Clone, Debug)]
pub struct VarDecl {
    pub ident: String,
    pub ty: Ty,
}

impl VarDecl {
    fn new(ident: String, ty: Ty) -> VarDecl {
        VarDecl { ident, ty }
    }
}

#[derive(Clone)]
pub struct VarStack {
    var_stack: Vec<usize>,
    vars: Vec<VarDecl>,
}

impl VarStack {
    pub fn new() -> VarStack {
        VarStack {
            var_stack: vec![],
            vars: vec![],
        }
    }

    fn add_var(&mut self, ident: String, ty: Ty) -> usize {
        self.vars.push(VarDecl::new(ident, ty));
        let pos = self.vars.len() - 1;
        self.var_stack.push(pos);
        pos
    }

    //TODO: this probably should be a Result in the future
    fn find_var(&self, ident: &str) -> Option<usize> {
        for var in self.var_stack.iter().rev() {
            if ident == &self.vars[*var].ident {
                return Some(*var);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub params: Vec<Param>,
    pub return_ty: Ty,
    pub vars: Vec<VarDecl>,
    pub bytecode: Vec<Bytecode>,
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub scope_id: ScopeId,
}
impl Mod {
    pub fn new(scope_id: ScopeId) -> Mod {
        Mod { scope_id }
    }
}

type ScopeId = usize;
type DefinitionId = usize;

#[derive(Clone, Debug)]
pub enum Lazy {
    ItemFn(ItemFn),
    ItemMod(ItemMod),
}

#[derive(Clone, Debug)]
pub enum Processed {
    Fun(Fun),
    Mod(Mod),
}

#[derive(Clone, Debug)]
pub enum DefinitionState {
    Lazy(Lazy),
    Processed(Processed),
}

pub struct Scope {
    parent: Option<ScopeId>,
    is_mod: bool,
    pub definitions: HashMap<String, DefinitionId>,
}

impl Scope {
    fn new(parent: Option<ScopeId>, is_mod: bool) -> Scope {
        Scope {
            parent,
            is_mod,
            definitions: HashMap::new(),
        }
    }
}

pub struct BytecodeEngine {
    pub scopes: Vec<Scope>,
    pub definitions: Vec<DefinitionState>,
}

fn operator_compatible(lhs: &Ty, rhs: &Ty) -> bool {
    match (lhs, rhs) {
        (Ty::U64, Ty::U64)
        | (Ty::U32, Ty::U32)
        | (Ty::U64, Ty::UnknownInt)
        | (Ty::U32, Ty::UnknownInt)
        | (Ty::UnknownInt, Ty::U64)
        | (Ty::UnknownInt, Ty::U32)
        | (Ty::UnknownInt, Ty::UnknownInt) => true,
        _ => false,
    }
}

fn assignment_compatible(lhs: &Ty, rhs: &Ty) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs, rhs) {
        (Ty::U64, Ty::UnknownInt)
        | (Ty::Unknown, _)
        | (Ty::U32, Ty::UnknownInt)
        | (Ty::UnknownInt, Ty::U64)
        | (Ty::UnknownInt, Ty::U32) => true,
        _ => false,
    }
}

fn tighter_of_types(lhs: &Ty, rhs: &Ty) -> Ty {
    match (lhs, rhs) {
        (Ty::U64, _) => Ty::U64,
        (Ty::U32, _) => Ty::U32,
        (Ty::Bool, _) => Ty::Bool,
        (_, Ty::U64) => Ty::U64,
        (_, Ty::U32) => Ty::U32,
        (_, Ty::Bool) => Ty::Bool,
        (Ty::Unknown, rhs) => rhs.clone(),
        (lhs, Ty::Unknown) => lhs.clone(),
        _ => lhs.clone(),
    }
}

impl BytecodeEngine {
    pub fn new() -> BytecodeEngine {
        BytecodeEngine {
            scopes: vec![
                Scope {
                    parent: None,
                    is_mod: true,
                    definitions: HashMap::new(),
                },
            ],
            definitions: vec![],
        }
    }

    /// Will find the definition id for the given name, by starting at the scope given and working up through the scopes
    /// until the matching definition is found.
    /// Returns the corresponding definition id with the scope it was found in
    fn get_defn(&self, defn_name: &str, starting_scope_id: ScopeId) -> (DefinitionId, ScopeId) {
        let mut current_scope_id = starting_scope_id;

        while !self.scopes[current_scope_id]
            .definitions
            .contains_key(defn_name)
        {
            if self.scopes[current_scope_id].is_mod {
                unimplemented!(
                    "Definition {} not found in module (or needs to be precomputed)",
                    defn_name
                );
            }
            if let Some(parent_id) = self.scopes[current_scope_id].parent {
                current_scope_id = parent_id;
            } else {
                unimplemented!("Definition {} needs to be precomputed", defn_name);
            }
        }

        (
            self.scopes[current_scope_id].definitions[defn_name],
            current_scope_id,
        )
    }

    /// Gets the bytecoded function for the given name
    pub fn get_fn(&self, defn_name: &str, scope_id: ScopeId) -> &Fun {
        let (defn_id, _) = self.get_defn(defn_name, scope_id);
        let defn = &self.definitions[defn_id];

        if let DefinitionState::Processed(Processed::Fun(ref p)) = defn {
            p
        } else {
            unimplemented!("Function {:?} needs to be precomputed", defn)
        }
    }

    pub fn load_file(&mut self, src: &String) {
        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");

        for item in syntax_file.items {
            match item {
                Item::Fn(item_fn) => {
                    let fn_name = item_fn.ident.to_string();
                    self.add_lazy_fn(fn_name, 0, item_fn);
                }
                Item::Mod(item_mod) => {
                    let mod_name = item_mod.ident.to_string();
                    self.add_lazy_mod(mod_name, 0, item_mod);
                }
                _ => {
                    unimplemented!("Unknown item type: {:#?}", item);
                }
            }
        }
    }

    fn add_lazy_fn(&mut self, fn_name: String, scope_id: ScopeId, item_fn: ItemFn) {
        self.definitions
            .push(DefinitionState::Lazy(Lazy::ItemFn(item_fn)));
        self.scopes[scope_id]
            .definitions
            .insert(fn_name, self.definitions.len() - 1);
    }

    fn add_lazy_mod(&mut self, mod_name: String, scope_id: ScopeId, item_mod: ItemMod) {
        self.definitions
            .push(DefinitionState::Lazy(Lazy::ItemMod(item_mod)));
        self.scopes[scope_id]
            .definitions
            .insert(mod_name, self.definitions.len() - 1);
    }

    pub fn process_fn(&mut self, fn_name: &str, scope_id: ScopeId) -> DefinitionId {
        let (definition_id, found_scope_id) = self.get_defn(fn_name, scope_id);

        let fun = self.convert_fn_to_bytecode(definition_id, found_scope_id);
        self.definitions[definition_id] = DefinitionState::Processed(Processed::Fun(fun));

        definition_id
    }

    fn process_mod(&mut self, mod_name: &str, scope_id: ScopeId) -> DefinitionId {
        let (definition_id, current_scope_id) = self.get_defn(mod_name, scope_id);

        if let DefinitionState::Lazy(Lazy::ItemMod(ref item_mod)) = self.definitions[definition_id]
        {
            self.scopes.push(Scope::new(Some(current_scope_id), true));
            let mod_scope_id = self.scopes.len() - 1;

            match item_mod.content {
                //TODO: would be great if we didn't clone here and just reused what we had
                Some(ref content) => for item in content.1.clone() {
                    match item {
                        Item::Fn(item_fn) => {
                            let fn_name = item_fn.ident.to_string();
                            self.add_lazy_fn(fn_name, mod_scope_id, item_fn);
                        }
                        Item::Mod(item_mod) => {
                            let mod_name = item_mod.ident.to_string();
                            self.add_lazy_mod(mod_name, mod_scope_id, item_mod);
                        }
                        _ => unimplemented!("Unsupport item type in module"),
                    }
                },
                None => {}
            }

            self.definitions[definition_id] =
                DefinitionState::Processed(Processed::Mod(Mod::new(mod_scope_id)));
        }
        definition_id
    }

    fn process_defn(&mut self, name: &str, scope_id: ScopeId) -> DefinitionId {
        let (definition_id, scope_id) = self.get_defn(name, scope_id);

        if let DefinitionState::Lazy(ref lazy) = self.definitions[definition_id] {
            match lazy {
                Lazy::ItemFn(_) => self.process_fn(name, scope_id),
                Lazy::ItemMod(_) => self.process_mod(name, scope_id),
            }
        } else {
            definition_id
        }
    }

    /// Processes a path looking for the definition being referenced.
    /// Returns: The processed definition id of the found item
    fn process_path(&mut self, path: &syn::Path, current_scope_id: ScopeId) -> DefinitionId {
        let mut mod_scope_id = current_scope_id;
        if path.leading_colon.is_some() {
            loop {
                if let Some(parent_id) = self.scopes[mod_scope_id].parent {
                    mod_scope_id = parent_id;
                } else {
                    break;
                }
            }
        }

        let num_segments = path.segments.len();

        for current_segment in 0..(num_segments - 1) {
            let ident = path.segments[current_segment].ident.as_ref();
            let definition_id = self.process_mod(ident, mod_scope_id);
            if let DefinitionState::Processed(Processed::Mod(ref module)) =
                self.definitions[definition_id]
            {
                mod_scope_id = module.scope_id;
            } else {
                unimplemented!("Failure to process module");
            }
        }

        // from there, look in this scpoe for the name
        let num_segments = path.segments.len();
        let ident = path.segments[num_segments - 1].ident.to_string();

        // lastly, make sure we've processed the definition before we return
        self.process_defn(&ident, mod_scope_id)
    }

    fn resolve_type(&mut self, tp: &Type) -> Ty {
        match *tp {
            Type::Path(ref tp) => match tp.path.segments[0].ident.as_ref() {
                "u64" => Ty::U64,
                "u32" => Ty::U32,
                "bool" => Ty::Bool,
                _ => Ty::Error,
            },
            _ => Ty::Error,
        }
    }

    pub fn convert_expr_to_bytecode(
        &mut self,
        expr: &Expr,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        current_scope_id: ScopeId,
        var_stack: &mut VarStack,
    ) -> Ty {
        match expr {
            Expr::Return(er) => {
                let actual_return_type = match er.expr {
                    Some(ref inner) => self.convert_expr_to_bytecode(
                        inner,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    ),
                    None => Ty::Void,
                };

                if assignment_compatible(expected_return_type, &actual_return_type) {
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
                Lit::Int(ref li) => match li.suffix() {
                    IntSuffix::U64 => {
                        bytecode.push(Bytecode::PushU64(li.value()));
                        Ty::U64
                    }
                    IntSuffix::U32 => {
                        bytecode.push(Bytecode::PushU32(li.value() as u32));
                        Ty::U32
                    }
                    _ => {
                        bytecode.push(Bytecode::PushU64(li.value()));
                        Ty::UnknownInt
                    }
                },
                Lit::Bool(ref lb) => {
                    bytecode.push(Bytecode::PushBool(lb.value));
                    Ty::Bool
                }
                _ => unimplemented!("unknown literal: {:?}", el),
            },
            Expr::Paren(ep) => self.convert_expr_to_bytecode(
                &*ep.expr,
                expected_return_type,
                bytecode,
                current_scope_id,
                var_stack,
            ),
            Expr::Block(eb) => self.convert_block_to_bytecode(
                &eb.block,
                expected_return_type,
                bytecode,
                Some(current_scope_id),
                var_stack,
            ),
            Expr::Assign(ea) => {
                let rhs_type = self.convert_expr_to_bytecode(
                    &*ea.right,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );

                match &*ea.left {
                    Expr::Path(ep) => {
                        let ident = ep.path.segments[0].ident.to_string();

                        let var_id = var_stack.find_var(&ident);
                        if var_id.is_none() {
                            unimplemented!("Could not find variable: {}", ident);
                        }
                        let var_id = var_id.unwrap();
                        let var = &mut var_stack.vars[var_id];

                        if assignment_compatible(&var.ty, &rhs_type) {
                            var.ty = tighter_of_types(&var.ty, &rhs_type);
                        } else {
                            unimplemented!("Assignment between {:?} and {:?}", var.ty, rhs_type);
                        }

                        bytecode.push(Bytecode::Assign(var_id));
                    }
                    _ => unimplemented!("Unsupported variable path for assignment"),
                }

                Ty::Void
            }
            Expr::If(ei) => {
                let cond_type = self.convert_expr_to_bytecode(
                    &*ei.cond,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );

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
                    Some(current_scope_id),
                    var_stack,
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
                                Some(current_scope_id),
                                var_stack,
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

                let cond_type = self.convert_expr_to_bytecode(
                    &*ew.cond,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );

                if cond_type != Ty::Bool {
                    unimplemented!("If condition needs to be boolean");
                }

                bytecode.push(Bytecode::WhileCond(0));
                let before_block_len = bytecode.len();

                let while_ty = self.convert_block_to_bytecode(
                    &ew.body,
                    expected_return_type,
                    bytecode,
                    Some(current_scope_id),
                    var_stack,
                );

                let after_block_len = bytecode.len();
                bytecode.push(Bytecode::EndWhile(after_block_len - before_cond_len));

                // Patch the original offset to the correct offset
                bytecode[before_block_len - 1] =
                    Bytecode::WhileCond(after_block_len - before_block_len + 1);

                while_ty
            }
            Expr::Binary(eb) => match eb.op {
                BinOp::Add(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    if operator_compatible(&lhs_type, &rhs_type) {
                        bytecode.push(Bytecode::Add);
                        tighter_of_types(&lhs_type, &rhs_type)
                    } else {
                        unimplemented!("Can't add values of {:?} and {:?}", lhs_type, rhs_type);
                    }
                }
                BinOp::Sub(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    if operator_compatible(&lhs_type, &rhs_type) {
                        bytecode.push(Bytecode::Sub);
                        tighter_of_types(&lhs_type, &rhs_type)
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
                        current_scope_id,
                        var_stack,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    if operator_compatible(&lhs_type, &rhs_type) {
                        bytecode.push(Bytecode::Mul);
                        tighter_of_types(&lhs_type, &rhs_type)
                    } else {
                        unimplemented!(
                            "Can't multiply values of {:?} and {:?}",
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
                        current_scope_id,
                        var_stack,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    if operator_compatible(&lhs_type, &rhs_type) {
                        bytecode.push(Bytecode::Div);
                        tighter_of_types(&lhs_type, &rhs_type)
                    } else {
                        unimplemented!("Can't divide values of {:?} and {:?}", lhs_type, rhs_type);
                    }
                }
                BinOp::Lt(_a) => {
                    let lhs_type = self.convert_expr_to_bytecode(
                        &*eb.left,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                    let rhs_type = self.convert_expr_to_bytecode(
                        &*eb.right,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );

                    if operator_compatible(&lhs_type, &rhs_type) {
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

                let var_id = var_stack.find_var(&ident);
                if var_id.is_none() {
                    unimplemented!("Could not find {}", ident);
                }
                let var_id = var_id.unwrap();
                let var = &var_stack.vars[var_id];

                if var.ty == Ty::Unknown {
                    unimplemented!("{} used before being given a value", ident);
                }

                bytecode.push(Bytecode::Var(var_id));

                var.ty.clone()
            }
            Expr::Call(ec) => match *ec.func {
                Expr::Path(ref ep) => {
                    if ep.path.segments.len() == 1 && ep.path.segments[0].ident == "__debug__" {
                        self.convert_expr_to_bytecode(
                            &ec.args[0],
                            expected_return_type,
                            bytecode,
                            current_scope_id,
                            var_stack,
                        );
                        bytecode.push(Bytecode::DebugPrint);
                        Ty::Void
                    } else {
                        // If we're in a single ident path, check values in scope
                        if ep.path.segments.len() == 1 && ep.path.leading_colon.is_none() {
                            let ident = ep.path.segments[0].ident;
                            let var_result = var_stack.find_var(ident.as_ref());
                            if let Some(var_id) = var_result {
                                //TODO: FIXME: in the future check this for lambda
                                unimplemented!(
                                    "Can not call function on type {:?}",
                                    var_stack.vars[var_id].ty
                                );
                            }
                        }

                        let definition_id = self.process_path(&ep.path, current_scope_id);

                        if let DefinitionState::Processed(Processed::Fun(ref target_fn)) =
                            self.definitions[definition_id]
                        {
                            let return_ty = target_fn.return_ty.clone();

                            for arg in &ec.args {
                                self.convert_expr_to_bytecode(
                                    arg,
                                    expected_return_type,
                                    bytecode,
                                    current_scope_id,
                                    var_stack,
                                );
                            }

                            bytecode.push(Bytecode::Call(definition_id));

                            return_ty
                        } else {
                            unimplemented!(
                                "Processed function {:?} did not process correctly",
                                ep.path
                            );
                        }
                    }
                }
                _ => unimplemented!("unknown function call type: {:#?}", ec.func),
            },
            _ => unimplemented!("Unknown expr type: {:#?}", expr),
        }
    }

    pub fn convert_stmt_to_bytecode(
        &mut self,
        stmt: &Stmt,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        current_scope_id: ScopeId,
        var_stack: &mut VarStack,
    ) -> Ty {
        match stmt {
            Stmt::Semi(ref e, _) => {
                self.convert_expr_to_bytecode(
                    e,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );
                Ty::Void
            }
            Stmt::Expr(ref e) => self.convert_expr_to_bytecode(
                e,
                expected_return_type,
                bytecode,
                current_scope_id,
                var_stack,
            ),
            Stmt::Local(ref l) => {
                let ident = match l.pats[0] {
                    Pat::Ident(ref pi) => pi.ident.to_string(),
                    _ => unimplemented!("Unsupported pattern in variable declaration"),
                };
                match l.init {
                    Some(ref foo) => {
                        let rhs_ty = self.convert_expr_to_bytecode(
                            &*foo.1,
                            expected_return_type,
                            bytecode,
                            current_scope_id,
                            var_stack,
                        );

                        match l.ty {
                            None => {
                                let var_id = var_stack.add_var(ident, rhs_ty);
                                bytecode.push(Bytecode::VarDecl(var_id));
                                Ty::Void
                            }
                            Some(ref explicit_ty) => {
                                let var_ty = self.resolve_type(&*explicit_ty.1);

                                if !assignment_compatible(&var_ty, &rhs_ty) {
                                    unimplemented!(
                                        "Explicit variable type '{:?}' does not match expression type '{:?}'", var_ty, rhs_ty
                                    )
                                }

                                let var_id = var_stack.add_var(ident, var_ty);
                                bytecode.push(Bytecode::VarDecl(var_id));

                                Ty::Void
                            }
                        }
                    }
                    None => {
                        match l.ty {
                            None => {
                                let var_id = var_stack.add_var(ident, Ty::Unknown);
                                bytecode.push(Bytecode::VarDeclUninit(var_id));
                            }
                            Some(ref explicit_ty) => {
                                let var_ty = self.resolve_type(&*explicit_ty.1);

                                let var_id = var_stack.add_var(ident, var_ty);
                                bytecode.push(Bytecode::VarDeclUninit(var_id));
                            }
                        }

                        Ty::Void
                    }
                }
            }
            _ => Ty::Void, // ignore Item(s) as we've already processed them
        }
    }

    fn convert_block_to_bytecode(
        &mut self,
        block: &Block,
        expected_return_type: &Ty,
        bytecode: &mut Vec<Bytecode>,
        parent: Option<ScopeId>,
        var_stack: &mut VarStack,
    ) -> Ty {
        //TODO: there may be more efficient ways to do this, but this will do for now
        let mut block_var_stack = var_stack.clone();
        let mut return_ty = Ty::Void;
        self.scopes.push(Scope::new(parent, false));
        let current_scope_id = self.scopes.len() - 1;

        for stmt in &block.stmts {
            if let Stmt::Item(ref item) = stmt {
                match item {
                    Item::Fn(ref item_fn) => {
                        self.add_lazy_fn(
                            item_fn.ident.to_string(),
                            current_scope_id,
                            item_fn.clone(),
                        );
                    }
                    Item::Mod(ref item_mod) => self.add_lazy_mod(
                        item_mod.ident.to_string(),
                        current_scope_id,
                        item_mod.clone(),
                    ),
                    Item::Use(ref item_use) => {
                        let mut current = &item_use.tree;
                        let mut temp_scope_id = current_scope_id;

                        loop {
                            println!("Use walking through: {}", temp_scope_id);
                            if self.scopes[temp_scope_id].is_mod {
                                break;
                            }
                            if let Some(parent_id) = self.scopes[temp_scope_id].parent {
                                temp_scope_id = parent_id;
                            } else {
                                break;
                            }
                        }

                        loop {
                            match current {
                                syn::UseTree::Name(ref use_name) => {
                                    let definition_id =
                                        self.process_defn(use_name.ident.as_ref(), temp_scope_id);

                                    println!(
                                        "Putting {} {} in at {}",
                                        use_name.ident.as_ref(),
                                        definition_id,
                                        current_scope_id
                                    );
                                    self.scopes[current_scope_id]
                                        .definitions
                                        .insert(use_name.ident.to_string(), definition_id);
                                    break;
                                }
                                syn::UseTree::Path(ref use_path) => {
                                    let definition_id =
                                        self.process_mod(use_path.ident.as_ref(), temp_scope_id);
                                    if let DefinitionState::Processed(Processed::Mod(ref module)) =
                                        self.definitions[definition_id]
                                    {
                                        temp_scope_id = module.scope_id;
                                        current = &*use_path.tree;
                                    } else {
                                        unimplemented!("Expected module in use path");
                                    }
                                }
                                _ => unimplemented!("Unsupport 'use' tree type"),
                            }
                        }
                    }
                    _ => unimplemented!("Unsupported item type: {:?}", item),
                }
            }
        }

        for stmt in &block.stmts {
            return_ty = self.convert_stmt_to_bytecode(
                stmt,
                &expected_return_type,
                bytecode,
                current_scope_id,
                &mut block_var_stack,
            );
        }

        var_stack.vars = block_var_stack.vars;

        return_ty
    }

    fn convert_fn_to_bytecode(&mut self, definition_id: DefinitionId, scope_id: ScopeId) -> Fun {
        let defn_state = self.definitions[definition_id].clone();

        match defn_state {
            DefinitionState::Processed(Processed::Fun(fun)) => fun,
            DefinitionState::Lazy(Lazy::ItemFn(item_fn)) => {
                let mut bytecode = Vec::new();

                let return_ty = match &item_fn.decl.output {
                    ReturnType::Default => Ty::Void,
                    ReturnType::Type(_, ref box_ty) => self.resolve_type(box_ty),
                };

                let mut var_stack = VarStack::new();
                let mut params = vec![];

                // process function params
                for input in &item_fn.decl.inputs {
                    match input {
                        FnArg::Captured(ref capture) => {
                            match capture.pat {
                                Pat::Ident(ref pi) => {
                                    let ident = pi.ident.to_string();
                                    let ty = self.resolve_type(&capture.ty);
                                    let var_id = var_stack.add_var(ident.clone(), ty.clone());
                                    params.push(Param::new(ident, var_id, ty));
                                }
                                _ => {
                                    unimplemented!("Unsupported pattern type in function parameter")
                                }
                            };
                        }
                        _ => unimplemented!("Function argument of {:?} is not supported", input),
                    }
                }

                let block_ty = self.convert_block_to_bytecode(
                    &item_fn.block,
                    &return_ty,
                    &mut bytecode,
                    Some(scope_id),
                    &mut var_stack,
                );

                match block_ty {
                    Ty::Void => bytecode.push(Bytecode::ReturnVoid),
                    _ => bytecode.push(Bytecode::ReturnLastStackValue),
                }

                if !assignment_compatible(&return_ty, &block_ty) {
                    unimplemented!(
                        "Mismatched return types: {:?} and {:?}",
                        block_ty,
                        return_ty
                    );
                }

                Fun {
                    params,
                    return_ty,
                    vars: var_stack.vars,
                    bytecode,
                }
            }
            _ => unimplemented!("Could not find function"),
        }
    }
}
