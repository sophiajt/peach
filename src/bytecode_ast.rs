use bytecode::{Bytecode, BytecodeEngine, DefinitionId, DefinitionState, Fun, Lazy, Param,
               Processed, Scope, ScopeId, VarStack};
use syn::{BinOp, Block, Expr, FnArg, IntSuffix, Lit, Pat, ReturnType, Stmt, Type};
use typecheck::{assignment_compatible, operator_compatible, tighter_of_types, Ty};

impl BytecodeEngine {
    pub(crate) fn convert_fn_to_bytecode(
        &mut self,
        definition_id: DefinitionId,
        scope_id: ScopeId,
    ) -> Fun {
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

    pub(crate) fn convert_block_to_bytecode(
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
                self.prepare_item(item.clone(), current_scope_id);
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

    pub(crate) fn convert_stmt_to_bytecode(
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

    pub(crate) fn convert_expr_to_bytecode(
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
}
