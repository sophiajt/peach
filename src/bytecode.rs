use std::collections::HashMap;

use syn::{self, FnArg, ForeignItem, Item, ItemFn, ItemMod, ItemStruct, Pat, ReturnType};
use typecheck::{builtin_type, TypeChecker, TypeId};

pub(crate) type ScopeId = usize;
pub(crate) type DefinitionId = usize;

type VarId = usize;
type Offset = usize;

#[derive(Debug, Clone)]
pub enum Bytecode {
    ReturnLastStackValue,
    ReturnVoid,
    PushU64(u64),
    PushU32(u32),
    PushI64(i64),
    PushI32(i32),
    PushUnknownInt(i32),
    PushBool(bool),
    PushRawNullPtr,
    As(TypeId),
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Neg,
    Dot(String),
    VarDecl(VarId),
    VarDeclUninit(VarId),
    Var(VarId),
    Assign,
    Call(DefinitionId),
    If(Offset, TypeId), // Offset is number of bytecodes to jump forward if false.  Also includes the type of the result, if this is an expression
    Else(Offset, TypeId), // Offset is number of bytecodes to skip (aka jump forward). Also includes the type of the result, if this is an expression
    EndIf(TypeId),        //includes the type of the result, if this is an expression
    BeginWhile,
    WhileCond(Offset), // Offset is number of bytecodes to jump forward if false
    EndWhile(Offset),  // Offset is number of bytecodes to jump backward to return to start of while
    DebugPrint(TypeId),

    //lvalue
    LValueVar(VarId),
    LValueDot(String),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub(crate) var_id: VarId,
    pub type_id: TypeId,
}
impl Param {
    pub fn new(name: String, var_id: VarId, type_id: TypeId) -> Param {
        Param {
            name,
            var_id,
            type_id,
        }
    }
}

//TODO: should VarDecl and Param be merged?
#[derive(Clone, Debug, Ord, Eq, PartialOrd, PartialEq)]
pub struct VarDecl {
    pub ident: String,
    pub type_id: TypeId,
}

impl VarDecl {
    fn new(ident: String, type_id: TypeId) -> VarDecl {
        VarDecl { ident, type_id }
    }
}

#[derive(Clone)]
pub struct VarStack {
    var_stack: Vec<usize>,
    pub(crate) vars: Vec<VarDecl>,
}

impl VarStack {
    pub fn new() -> VarStack {
        VarStack {
            var_stack: vec![],
            vars: vec![],
        }
    }

    pub(crate) fn add_var(&mut self, ident: String, type_id: TypeId) -> usize {
        self.vars.push(VarDecl::new(ident, type_id));
        let pos = self.vars.len() - 1;
        self.var_stack.push(pos);
        pos
    }

    //TODO: this probably should be a Result in the future
    pub fn find_var(&self, ident: &str) -> Option<usize> {
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
    pub return_type_id: TypeId,
    pub vars: Vec<VarDecl>,
    pub bytecode: Vec<Bytecode>,
    pub extern_name: Option<String>,
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

#[derive(Debug, Clone)]
pub struct Struct {
    pub type_id: TypeId,
}
impl Struct {
    fn new(type_id: TypeId) -> Struct {
        Struct { type_id }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Lazy {
    ItemFn(ItemFn),
    ItemMod(ItemMod),
    ItemStruct(ItemStruct),
}

#[derive(Clone, Debug)]
pub(crate) enum Processed {
    Fun(Fun),
    Mod(Mod),
    Struct(Struct),
}

#[derive(Clone, Debug)]
pub(crate) enum Definition {
    Lazy(Lazy),
    Processed(Processed),
}

pub struct Scope {
    parent: Option<ScopeId>,
    is_mod: bool,
    pub(crate) definitions: HashMap<String, DefinitionId>,
}

impl Scope {
    pub(crate) fn new(parent: Option<ScopeId>, is_mod: bool) -> Scope {
        Scope {
            parent,
            is_mod,
            definitions: HashMap::new(),
        }
    }
}

/// BytecodeEngine is the root of Peach's work.  Here code is converted from source files to an intermediate bytecode format
/// First, the file is parsed into an AST.  Once an AST, further computation is delayed until definitions are required.
/// This allows conversion from AST to definitions to happen lazily.
///
/// No processing is done by default.  Once a file is loaded, you must then process the file by giving a function name to begin with.
/// Eg)
/// ```no_run
/// extern crate peachlib;
/// use peachlib::BytecodeEngine;
///
/// let mut bc = BytecodeEngine::new();
/// bc.load_file("bin.rs");
/// bc.process_fn("main", 0);
/// ```
/// Processing is done on function granularity.  As definitions are referenced in the function, they too are processed.
pub struct BytecodeEngine {
    pub(crate) scopes: Vec<Scope>,
    pub(crate) definitions: Vec<Definition>,
    pub(crate) project_root: Option<::std::path::PathBuf>,

    //TODO: FIXME: probably want to make this pub(crate) at some point
    pub typechecker: TypeChecker,
}

impl BytecodeEngine {
    pub fn new() -> BytecodeEngine {
        BytecodeEngine {
            scopes: vec![Scope {
                parent: None,
                is_mod: true,
                definitions: HashMap::new(),
            }],
            definitions: vec![],
            project_root: None,
            typechecker: TypeChecker::new(),
        }
    }

    /// Will find the definition id for the given name, by starting at the scope given and working up through the scopes
    /// until the matching definition is found.
    /// Returns the corresponding definition id with the scope it was found in
    pub(crate) fn get_defn(
        &self,
        defn_name: &str,
        starting_scope_id: ScopeId,
    ) -> Option<(DefinitionId, ScopeId)> {
        let mut current_scope_id = starting_scope_id;

        while !self.scopes[current_scope_id]
            .definitions
            .contains_key(defn_name)
        {
            if self.scopes[current_scope_id].is_mod {
                return None;
            }
            if let Some(parent_id) = self.scopes[current_scope_id].parent {
                current_scope_id = parent_id;
            } else {
                return None;
            }
        }

        Some((
            self.scopes[current_scope_id].definitions[defn_name],
            current_scope_id,
        ))
    }

    /// Gets the bytecoded function for the given name
    pub fn get_fn(&self, defn_name: &str, scope_id: ScopeId) -> &Fun {
        if let Some((defn_id, _)) = self.get_defn(defn_name, scope_id) {
            let defn = &self.definitions[defn_id];

            if let Definition::Processed(Processed::Fun(ref p)) = defn {
                p
            } else {
                unimplemented!("Function {:?} needs to be precomputed", defn)
            }
        } else {
            unimplemented!("Function {} could not be found", defn_name);
        }
    }

    /// Sets the project root that will be used when modules are loaded
    pub fn set_project_root(&mut self, path: &str) {
        use std::fs;

        let path = fs::canonicalize(path).unwrap();

        self.project_root = Some(path);
    }

    /// Loads the file with the given name
    pub fn load_file(&mut self, fname: &str) {
        use std::fs::File;
        use std::io::Read;
        let path = if let Some(ref project_path) = self.project_root {
            let mut temp_path = project_path.clone();
            temp_path.push(fname);
            temp_path
        } else {
            let mut temp_path = ::std::path::PathBuf::new();
            temp_path.push(fname);
            temp_path
        };

        let mut file = File::open(path).expect("Unable to open file");

        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");

        let syntax_file = syn::parse_file(&src).expect("Unable to parse file");

        for item in syntax_file.items {
            self.prepare_item(item, 0);
        }
    }

    /// Prepares the given item to be processed lazily
    pub fn prepare_item(&mut self, item: Item, current_scope_id: ScopeId) {
        use std::fs::File;
        use std::io::Read;

        match item {
            Item::Fn(item_fn) => {
                // Adds a function to be processed lazily
                let fn_name = item_fn.ident.to_string();
                self.definitions
                    .push(Definition::Lazy(Lazy::ItemFn(item_fn)));
                self.scopes[current_scope_id]
                    .definitions
                    .insert(fn_name, self.definitions.len() - 1);
            }
            Item::ForeignMod(item_fm) => for f in item_fm.items {
                match f {
                    ForeignItem::Fn(fun) => {
                        let fn_name = fun.ident.to_string();

                        let return_type_id = match &fun.decl.output {
                            ReturnType::Default => builtin_type::VOID,
                            ReturnType::Type(_, ref box_ty) => {
                                self.resolve_type(box_ty, current_scope_id)
                            }
                        };

                        let mut var_stack = VarStack::new();
                        let mut params = vec![];

                        // process function params
                        for input in &fun.decl.inputs {
                            match input {
                                FnArg::Captured(ref capture) => {
                                    match capture.pat {
                                        Pat::Ident(ref pi) => {
                                            let ident = pi.ident.to_string();
                                            let type_id =
                                                self.resolve_type(&capture.ty, current_scope_id);
                                            let var_id = var_stack.add_var(ident.clone(), type_id);
                                            params.push(Param::new(ident, var_id, type_id));
                                        }
                                        _ => unimplemented!(
                                            "Unsupported pattern type in function parameter"
                                        ),
                                    };
                                }
                                _ => unimplemented!(
                                    "Function argument of {:?} is not supported",
                                    input
                                ),
                            }
                        }

                        self.definitions
                            .push(Definition::Processed(Processed::Fun(Fun {
                                bytecode: vec![],
                                params,
                                return_type_id,
                                vars: vec![],
                                extern_name: Some(fn_name.clone()),
                            })));
                        self.scopes[current_scope_id]
                            .definitions
                            .insert(fn_name, self.definitions.len() - 1);
                    }
                    _ => unimplemented!("Unsupported foreign item"),
                }
            },
            Item::Mod(item_mod) => {
                if item_mod.content.is_none() {
                    //Load the file as a module
                    let fname = item_mod.ident.as_ref();
                    let path = if let Some(ref project_path) = self.project_root {
                        let mut temp_path = project_path.clone();
                        temp_path.push(fname);
                        temp_path.set_extension("rs");
                        temp_path
                    } else {
                        let mut temp_path = ::std::path::PathBuf::new();
                        temp_path.push(fname);
                        temp_path.set_extension("rs");
                        temp_path
                    };

                    let mut file = File::open(path).expect("Unable to open file");

                    let mut src = String::new();
                    file.read_to_string(&mut src).expect("Unable to read file");

                    let syntax_file = syn::parse_file(&src).expect("Unable to parse file");
                    self.scopes.push(Scope::new(None, true));
                    let mod_scope_id = self.scopes.len() - 1;

                    // Eagerly process the top-most bit of the file as a module
                    // This allows us to make its contents lazily available
                    // Part of the reason we do it this way is that we don't have an ItemMod
                    self.definitions
                        .push(Definition::Processed(Processed::Mod(Mod::new(
                            mod_scope_id,
                        ))));

                    self.scopes[current_scope_id]
                        .definitions
                        .insert(item_mod.ident.to_string(), self.definitions.len() - 1);

                    for item in syntax_file.items {
                        self.prepare_item(item, mod_scope_id);
                    }
                } else {
                    // Add module to be processed lazily
                    let mod_name = item_mod.ident.to_string();
                    self.definitions
                        .push(Definition::Lazy(Lazy::ItemMod(item_mod)));
                    self.scopes[current_scope_id]
                        .definitions
                        .insert(mod_name, self.definitions.len() - 1);
                }
            }
            Item::Use(ref item_use) => {
                // Use seems to start higher up in the scopes, so start higher
                let mut temp_scope_id = current_scope_id;

                loop {
                    //TODO: FIXME: not sure if this is correct
                    if self.scopes[temp_scope_id].is_mod {
                        break;
                    }
                    if let Some(parent_id) = self.scopes[temp_scope_id].parent {
                        temp_scope_id = parent_id;
                    } else {
                        break;
                    }
                }

                self.process_use_tree(&item_use.tree, current_scope_id, temp_scope_id);
            }
            Item::Struct(item_struct) => {
                let ident = item_struct.ident.to_string();

                self.definitions
                    .push(Definition::Lazy(Lazy::ItemStruct(item_struct)));
                self.scopes[current_scope_id]
                    .definitions
                    .insert(ident, self.definitions.len() - 1);
            }
            _ => {
                unimplemented!("Unknown item type: {:#?}", item);
            }
        }
    }

    /// Begin processing the lazy definitions starting at the given function.
    /// This will continue processing until all necessary definitions have been processed.
    pub fn process_fn(&mut self, fn_name: &str, scope_id: ScopeId) -> DefinitionId {
        if let Some((definition_id, found_scope_id)) = self.get_defn(fn_name, scope_id) {
            let fun = self.convert_fn_to_bytecode(definition_id, found_scope_id);
            self.definitions[definition_id] = Definition::Processed(Processed::Fun(fun));

            definition_id
        } else {
            unimplemented!("Can not find function {}", fn_name);
        }
    }

    fn process_struct(&mut self, struct_name: &str, scope_id: ScopeId) -> DefinitionId {
        if let Some((definition_id, _found_scope_id)) = self.get_defn(struct_name, scope_id) {
            let fields_in = if let Definition::Lazy(Lazy::ItemStruct(ref item_struct)) =
                self.definitions[definition_id]
            {
                item_struct.fields.clone()
            } else {
                unimplemented!("Could not process struct fields");
            };

            let mut fields: Vec<(String, TypeId)> = vec![];
            for iter in &fields_in {
                let field_ty = self.resolve_type(&iter.ty, scope_id);
                fields.push((iter.ident.unwrap().to_string(), field_ty));
            }

            fields.sort();

            let type_id = self.typechecker.new_struct(fields);
            let s = Struct::new(type_id);
            self.definitions[definition_id] = Definition::Processed(Processed::Struct(s));

            definition_id
        } else {
            unimplemented!("Can not find struct {}", struct_name);
        }
    }

    fn process_mod(&mut self, mod_name: &str, scope_id: ScopeId) -> DefinitionId {
        if let Some((definition_id, current_scope_id)) = self.get_defn(mod_name, scope_id) {
            if let Definition::Lazy(Lazy::ItemMod(ref item_mod)) = self.definitions[definition_id] {
                self.scopes.push(Scope::new(Some(current_scope_id), true));
                let mod_scope_id = self.scopes.len() - 1;

                match item_mod.content {
                    //TODO: would be great if we didn't clone here and just reused what we had
                    Some(ref content) => for item in content.1.clone() {
                        self.prepare_item(item, mod_scope_id);
                    },
                    None => {}
                }

                self.definitions[definition_id] =
                    Definition::Processed(Processed::Mod(Mod::new(mod_scope_id)));
            }
            definition_id
        } else {
            unimplemented!("Can not find mod {}", mod_name);
        }
    }

    fn process_defn(&mut self, name: &str, scope_id: ScopeId) -> Option<DefinitionId> {
        if let Some((definition_id, scope_id)) = self.get_defn(name, scope_id) {
            if let Definition::Lazy(ref lazy) = self.definitions[definition_id] {
                let result = match lazy {
                    Lazy::ItemFn(_) => self.process_fn(name, scope_id),
                    Lazy::ItemMod(_) => self.process_mod(name, scope_id),
                    Lazy::ItemStruct(_) => self.process_struct(name, scope_id),
                };
                Some(result)
            } else {
                Some(definition_id)
            }
        } else {
            None
        }
    }

    /// Processes a path looking for the definition being referenced.
    /// Returns: The processed definition id of the found item
    pub(crate) fn process_path(
        &mut self,
        path: &syn::Path,
        current_scope_id: ScopeId,
    ) -> Option<DefinitionId> {
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
            if let Definition::Processed(Processed::Mod(ref module)) =
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

    fn process_use_tree(
        &mut self,
        use_tree: &syn::UseTree,
        original_scope_id: ScopeId,
        current_scope_id: ScopeId,
    ) {
        match use_tree {
            syn::UseTree::Name(ref use_name) => {
                let definition_id = self.process_defn(use_name.ident.as_ref(), current_scope_id);

                if definition_id.is_none() {
                    unimplemented!(
                        "Could not process the definition for {}",
                        use_name.ident.as_ref()
                    );
                }

                self.scopes[original_scope_id]
                    .definitions
                    .insert(use_name.ident.to_string(), definition_id.unwrap());
            }
            syn::UseTree::Path(ref use_path) => {
                let definition_id = self.process_mod(use_path.ident.as_ref(), current_scope_id);
                if let Definition::Processed(Processed::Mod(ref module)) =
                    self.definitions[definition_id]
                {
                    self.process_use_tree(&*use_path.tree, original_scope_id, module.scope_id);
                } else {
                    unimplemented!("Expected module in use path");
                }
            }
            syn::UseTree::Group(ref use_group) => {
                for tree in &use_group.items {
                    self.process_use_tree(tree, original_scope_id, current_scope_id);
                }
            }
            syn::UseTree::Glob(_) => {
                let mut defn_names = vec![];
                for defn_name in self.scopes[current_scope_id].definitions.keys() {
                    defn_names.push(defn_name.clone());
                }

                for defn_name in defn_names {
                    let definition_id = self.process_defn(&defn_name, current_scope_id);

                    if definition_id.is_none() {
                        unimplemented!("Could not process the definition for {}", defn_name);
                    }

                    self.scopes[original_scope_id]
                        .definitions
                        .insert(defn_name, definition_id.unwrap());
                }
            }
            syn::UseTree::Rename(ref use_rename) => {
                let definition_id = self.process_defn(use_rename.ident.as_ref(), current_scope_id);

                if definition_id.is_none() {
                    unimplemented!(
                        "Could not process the definition for {}",
                        use_rename.ident.as_ref()
                    );
                }

                self.scopes[original_scope_id]
                    .definitions
                    .insert(use_rename.rename.to_string(), definition_id.unwrap());
            }
        }
    }

    /// immediately process a string into bytecode, treating it as an expression
    /// this is likely only useful for building REPLs
    pub fn process_raw_expr_str(
        &mut self,
        expr_str: &str,
        bytecode: &mut Vec<Bytecode>,
        var_stack: &mut VarStack,
    ) -> Result<TypeId, String> {
        match syn::parse_str::<syn::Expr>(expr_str) {
            Ok(expr) => {
                Ok(self.convert_expr_to_bytecode(
                    &expr,
                    builtin_type::UNKNOWN,
                    bytecode,
                    0, // hardwire repl scope to 0
                    var_stack,
                ))
            }
            Err(e) => Err(e.to_string()),
        }
    }

    /// immediately process a string into bytecode, treating it as a statement
    /// this will also process items so that their definitions are in scope
    /// this is likely only useful for building REPLs
    pub fn process_raw_stmt_str(
        &mut self,
        expr_str: &str,
        bytecode: &mut Vec<Bytecode>,
        var_stack: &mut VarStack,
    ) -> Result<(), String> {
        match syn::parse_str::<syn::Stmt>(expr_str) {
            Ok(stmt) => {
                match stmt {
                    syn::Stmt::Item(item) => {
                        self.prepare_item(item, 0);
                        Ok(())
                    }
                    _ => {
                        self.convert_stmt_to_bytecode(
                            &stmt,
                            builtin_type::UNKNOWN,
                            bytecode,
                            0, // hardwire repl scope to 0
                            var_stack,
                        );
                        Ok(())
                    }
                }
            }
            Err(e) => Err(e.to_string()),
        }
    }
}
