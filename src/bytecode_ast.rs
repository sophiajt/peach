use bytecode::{Bytecode, BytecodeEngine, Definition, DefinitionId, Fun, Lazy, Param, Processed,
               Scope, ScopeId, VarStack};
use syn::{BinOp, Block, Expr, FnArg, IntSuffix, Lit, Member, Pat, ReturnType, Stmt, Type};
use typecheck::{builtin_type, TypeId, TypeInfo};

impl BytecodeEngine {
    pub(crate) fn convert_fn_to_bytecode(
        &mut self,
        definition_id: DefinitionId,
        scope_id: ScopeId,
    ) -> Fun {
        let defn_state = self.definitions[definition_id].clone();

        match defn_state {
            Definition::Processed(Processed::Fun(fun)) => fun,
            Definition::Lazy(Lazy::ItemFn(item_fn)) => {
                let mut bytecode = Vec::new();

                let return_type_id = match &item_fn.decl.output {
                    ReturnType::Default => builtin_type::VOID,
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
                                    let type_id = self.resolve_type(&capture.ty);
                                    let var_id = var_stack.add_var(ident.clone(), type_id);
                                    params.push(Param::new(ident, var_id, type_id));
                                }
                                _ => {
                                    unimplemented!("Unsupported pattern type in function parameter")
                                }
                            };
                        }
                        _ => unimplemented!("Function argument of {:?} is not supported", input),
                    }
                }

                let block_type_id = self.convert_block_to_bytecode(
                    &item_fn.block,
                    return_type_id,
                    &mut bytecode,
                    Some(scope_id),
                    &mut var_stack,
                );

                match block_type_id {
                    builtin_type::VOID => bytecode.push(Bytecode::ReturnVoid),
                    _ => bytecode.push(Bytecode::ReturnLastStackValue),
                }

                if !self.typechecker
                    .assignment_compatible(return_type_id, block_type_id)
                {
                    unimplemented!(
                        "Mismatched return types: {:?} and {:?}",
                        block_type_id,
                        return_type_id
                    );
                }

                Fun {
                    params,
                    return_type_id,
                    vars: var_stack.vars,
                    bytecode,
                }
            }
            _ => unimplemented!("Could not find function"),
        }
    }

    pub(crate) fn convert_block_to_bytecode(
        &mut self,
        block: &Block,
        expected_return_type: TypeId,
        bytecode: &mut Vec<Bytecode>,
        parent: Option<ScopeId>,
        var_stack: &mut VarStack,
    ) -> TypeId {
        //TODO: there may be more efficient ways to do this, but this will do for now
        let mut block_var_stack = var_stack.clone();
        let mut return_type_id = builtin_type::VOID;
        self.scopes.push(Scope::new(parent, false));
        let current_scope_id = self.scopes.len() - 1;

        for stmt in &block.stmts {
            if let Stmt::Item(ref item) = stmt {
                self.prepare_item(item.clone(), current_scope_id);
            }
        }

        for stmt in &block.stmts {
            return_type_id = self.convert_stmt_to_bytecode(
                stmt,
                expected_return_type,
                bytecode,
                current_scope_id,
                &mut block_var_stack,
            );
        }

        var_stack.vars = block_var_stack.vars;

        return_type_id
    }

    pub fn convert_stmt_to_bytecode(
        &mut self,
        stmt: &Stmt,
        expected_return_type: TypeId,
        bytecode: &mut Vec<Bytecode>,
        current_scope_id: ScopeId,
        var_stack: &mut VarStack,
    ) -> TypeId {
        match stmt {
            Stmt::Semi(ref e, _) => {
                self.convert_expr_to_bytecode(
                    e,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );
                builtin_type::VOID
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
                                builtin_type::VOID
                            }
                            Some(ref explicit_ty) => {
                                let var_ty = self.resolve_type(&*explicit_ty.1);

                                if !self.typechecker.assignment_compatible(var_ty, rhs_ty) {
                                    unimplemented!(
                                        "Explicit variable type '{:?}' does not match expression type '{:?}'", var_ty, rhs_ty
                                    )
                                }

                                let var_id = var_stack.add_var(ident, var_ty);
                                bytecode.push(Bytecode::VarDecl(var_id));

                                builtin_type::VOID
                            }
                        }
                    }
                    None => {
                        match l.ty {
                            None => {
                                let var_id = var_stack.add_var(ident, builtin_type::UNKNOWN);
                                bytecode.push(Bytecode::VarDeclUninit(var_id));
                            }
                            Some(ref explicit_ty) => {
                                let var_ty = self.resolve_type(&*explicit_ty.1);

                                let var_id = var_stack.add_var(ident, var_ty);
                                bytecode.push(Bytecode::VarDeclUninit(var_id));
                            }
                        }

                        builtin_type::VOID
                    }
                }
            }
            _ => builtin_type::VOID, // ignore Item(s) as we've already processed them
        }
    }

    pub fn convert_expr_to_bytecode(
        &mut self,
        expr: &Expr,
        expected_return_type: TypeId,
        bytecode: &mut Vec<Bytecode>,
        current_scope_id: ScopeId,
        var_stack: &mut VarStack,
    ) -> TypeId {
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
                    None => builtin_type::VOID,
                };

                if self.typechecker
                    .assignment_compatible(expected_return_type, actual_return_type)
                {
                    match actual_return_type {
                        builtin_type::VOID => bytecode.push(Bytecode::ReturnVoid),
                        _ => bytecode.push(Bytecode::ReturnLastStackValue),
                    }
                    builtin_type::VOID
                } else {
                    println!(
                        "Mismatched return types: {:?} and {:?}",
                        actual_return_type, expected_return_type
                    );
                    builtin_type::ERROR
                }
            }
            Expr::Lit(el) => match el.lit {
                Lit::Int(ref li) => match li.suffix() {
                    IntSuffix::U64 => {
                        bytecode.push(Bytecode::PushU64(li.value()));
                        builtin_type::U64
                    }
                    IntSuffix::U32 => {
                        bytecode.push(Bytecode::PushU32(li.value() as u32));
                        builtin_type::U32
                    }
                    _ => {
                        bytecode.push(Bytecode::PushU64(li.value()));
                        builtin_type::UNKNOWN_INT
                    }
                },
                Lit::Bool(ref lb) => {
                    bytecode.push(Bytecode::PushBool(lb.value));
                    builtin_type::BOOL
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

                        if self.typechecker
                            .assignment_compatible(var.type_id, rhs_type)
                        {
                            var.type_id = self.typechecker.tighter_of_types(var.type_id, rhs_type);
                        } else {
                            unimplemented!(
                                "Assignment between {:?} and {:?}",
                                var.type_id,
                                rhs_type
                            );
                        }

                        bytecode.push(Bytecode::Assign(var_id));
                    }
                    _ => unimplemented!("Unsupported variable path for assignment"),
                }

                builtin_type::VOID
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
                    builtin_type::BOOL => {}
                    _ => unimplemented!("If condition needs to be boolean"),
                }

                bytecode.push(Bytecode::If(0, builtin_type::VOID));
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
                    bytecode.push(Bytecode::Else(0, builtin_type::VOID));
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
                bytecode.push(Bytecode::EndIf(then_ty));

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

                if cond_type != builtin_type::BOOL {
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
                    if self.typechecker.operator_compatible(lhs_type, rhs_type) {
                        bytecode.push(Bytecode::Add);
                        self.typechecker.tighter_of_types(lhs_type, rhs_type)
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
                    if self.typechecker.operator_compatible(lhs_type, rhs_type) {
                        bytecode.push(Bytecode::Sub);
                        self.typechecker.tighter_of_types(lhs_type, rhs_type)
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
                    if self.typechecker.operator_compatible(lhs_type, rhs_type) {
                        bytecode.push(Bytecode::Mul);
                        self.typechecker.tighter_of_types(lhs_type, rhs_type)
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
                    if self.typechecker.operator_compatible(lhs_type, rhs_type) {
                        bytecode.push(Bytecode::Div);
                        self.typechecker.tighter_of_types(lhs_type, rhs_type)
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

                    if self.typechecker.operator_compatible(lhs_type, rhs_type) {
                        bytecode.push(Bytecode::Lt);
                        builtin_type::BOOL
                    } else {
                        unimplemented!("Can't compare values of {:?} and {:?}", lhs_type, rhs_type);
                    }
                }
                _ => unimplemented!("Unknown operator: {:?}", eb.op),
            },
            Expr::Struct(es) => {
                let mut fields = vec![];
                for field in &es.fields {
                    match field.member {
                        Member::Named(name) => {
                            fields.push((name.to_string(), &field.expr));
                        }
                        _ => unimplemented!("Unnamed struct members not yet supported"),
                    }
                }

                //TODO: FIXME: would be great to not have to clone here
                fields.sort_by_key(|x| x.0.clone());

                for field in fields {
                    self.convert_expr_to_bytecode(
                        field.1,
                        expected_return_type,
                        bytecode,
                        current_scope_id,
                        var_stack,
                    );
                }
                if let Some(definition_id) = self.process_path(&es.path, current_scope_id) {
                    if let Definition::Processed(Processed::Struct(ref s)) =
                        self.definitions[definition_id]
                    {
                        bytecode.push(Bytecode::Call(definition_id));
                        s.type_id
                    } else {
                        unimplemented!("Unsupport definition type in struct call");
                    }
                } else {
                    unimplemented!("Can't process struct");
                }
            }
            Expr::Path(ep) => {
                if let Some(definition_id) = self.process_path(&ep.path, current_scope_id) {
                    if let Definition::Processed(Processed::Struct(ref s)) =
                        self.definitions[definition_id]
                    {
                        bytecode.push(Bytecode::Call(definition_id));
                        s.type_id
                    } else {
                        unimplemented!("Unsupport definition type in struct call");
                    }
                } else {
                    let ident = ep.path.segments[0].ident.to_string();
                    let var_id = var_stack.find_var(&ident);
                    if var_id.is_none() {
                        unimplemented!("Could not find {}", ident);
                    }
                    let var_id = var_id.unwrap();
                    let var = &var_stack.vars[var_id];

                    if var.type_id == builtin_type::UNKNOWN {
                        unimplemented!("{} used before being given a value", ident);
                    }

                    bytecode.push(Bytecode::Var(var_id));

                    var.type_id
                }
            }
            Expr::Call(ec) => match *ec.func {
                Expr::Path(ref ep) => {
                    if ep.path.segments.len() == 1 && ep.path.segments[0].ident == "__debug__" {
                        let type_id = self.convert_expr_to_bytecode(
                            &ec.args[0],
                            expected_return_type,
                            bytecode,
                            current_scope_id,
                            var_stack,
                        );
                        bytecode.push(Bytecode::DebugPrint(type_id));
                        builtin_type::VOID
                    } else {
                        // If we're in a single ident path, check values in scope
                        if ep.path.segments.len() == 1 && ep.path.leading_colon.is_none() {
                            let ident = ep.path.segments[0].ident;
                            let var_result = var_stack.find_var(ident.as_ref());
                            if let Some(var_id) = var_result {
                                //TODO: FIXME: in the future check this for lambda
                                unimplemented!(
                                    "Can not call function on type {:?}",
                                    var_stack.vars[var_id].type_id
                                );
                            }
                        }

                        let definition_id = self.process_path(&ep.path, current_scope_id);

                        if definition_id.is_none() {
                            unimplemented!("Could not find call for {:?}", ep.path);
                        }

                        //TODO: FIXME: please don't do this
                        let definition_id = definition_id.unwrap();

                        if let Definition::Processed(Processed::Fun(ref target_fn)) =
                            self.definitions[definition_id]
                        {
                            let return_type_id = target_fn.return_type_id;

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

                            return_type_id
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
            Expr::Field(ef) => {
                let type_id = self.convert_expr_to_bytecode(
                    &*ef.base,
                    expected_return_type,
                    bytecode,
                    current_scope_id,
                    var_stack,
                );

                if let TypeInfo::Struct(ref st) = self.typechecker.types[type_id] {
                    match ef.member {
                        Member::Named(ident) => {
                            bytecode.push(Bytecode::Dot(ident.to_string()));
                            for field in &st.fields {
                                if field.0 == ident.as_ref() {
                                    return field.1;
                                }
                            }
                            unimplemented!("Field access of {} not found", ident);
                        }
                        _ => unimplemented!("Unsupported member access"),
                    }
                } else {
                    unimplemented!("Member access on non-struct types");
                }
            }
            _ => unimplemented!("Unknown expr type: {:#?}", expr),
        }
    }

    pub(crate) fn resolve_type(&self, tp: &Type) -> TypeId {
        match *tp {
            Type::Path(ref tp) => match tp.path.segments[0].ident.as_ref() {
                "u64" => builtin_type::U64,
                "u32" => builtin_type::U32,
                "bool" => builtin_type::BOOL,
                _ => builtin_type::ERROR,
            },
            _ => builtin_type::ERROR,
        }
    }
}
