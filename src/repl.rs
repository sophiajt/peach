use bytecode::{self, Bytecode, BytecodeEngine};
use eval::{Value, eval_block_bytecode};
use typecheck::Ty;
use std::collections::HashMap;
use syn;

pub fn repl() {
    use std::io::{stdin, stdout, Write};

    let mut bc = BytecodeEngine::new();
    let mut var_stack = bytecode::VarStack::new();
    let mut var_lookup: HashMap<usize, usize> = HashMap::new();
    let mut value_stack: Vec<Value> = vec![];
    let mut show_type = false;
    let mut show_bytecode = false;

    //TODO: FIXME: don't hardcode this
    bc.set_project_root("test_files");

    println!("madness repl (:h for help, :q to quit)");
    loop {
        let mut bytecode: Vec<Bytecode> = vec![];
        let mut input = String::new();

        print!("> ");
        let _ = stdout().flush();
        stdin().read_line(&mut input).expect("Could not read input");
        input = input.trim().to_string();

        if input == ":quit" || input == ":q" {
            break;
        }

        if input == ":type" || input == ":t" {
            show_type ^= true;
            println!("show type: {}", show_type);
            continue;
        }

        if input == ":bytecode" || input == ":b" {
            show_bytecode ^= true;
            println!("show bytecode: {}", show_bytecode);
            continue;
        }

        if input == ":stack" || input == ":s" {
            println!("{:?}", value_stack);
            continue;
        }

        if input == ":help" || input == ":h" {
            println!(":h(elp) - print this help message");
            println!(":t(ype) - print the result type");
            println!(":b(ytecode) - print the bytecode");
            println!(":s(tack) - print the contents of the stack");
            println!(":q(uit) - quit repl");
            continue;
        }

        let result = syn::parse_str::<syn::Expr>(&input);
        match result {
            Ok(expr) => {
                let ty = bc.convert_expr_to_bytecode(
                    &expr,
                    &Ty::Unknown,
                    &mut bytecode,
                    0, // hardwire repl scope to 0
                    &mut var_stack,
                );

                if show_type {
                    println!("type: {}", ty);
                }
                if show_bytecode {
                    println!("bytecode: {:?}", bytecode);
                }
                eval_block_bytecode(&bc, &bytecode, &mut var_lookup, &mut value_stack, &mut None);

                // This funny little trick should, in theory, let us pop off temporaries without popping off our variables
                let last = if value_stack.len() > var_lookup.len() {
                    value_stack.pop().unwrap()
                } else {
                    value_stack.last().unwrap().clone()
                };

                println!("{}", last);
            }
            _ => {
                if input.chars().rev().next() != Some(';') {
                    input.push(';');
                }
                let result = syn::parse_str::<syn::Stmt>(&input);
                match result {
                    Ok(result) => {
                        //TODO: FIXME: we should probably have something that can handle items *and* stmts for the repl
                        match result {
                            syn::Stmt::Item(item) => {
                                bc.prepare_item(item, 0);
                            }
                            _ => {
                                let ty = bc.convert_stmt_to_bytecode(
                                    &result,
                                    &Ty::Unknown,
                                    &mut bytecode,
                                    0, // hardwire repl scope to 0
                                    &mut var_stack,
                                );
                                if show_type {
                                    println!("type: {}", ty);
                                }
                                if show_bytecode {
                                    println!("bytecode: {:?}", bytecode);
                                }
                                eval_block_bytecode(
                                    &bc,
                                    &bytecode,
                                    &mut var_lookup,
                                    &mut value_stack,
                                    &mut None,
                                );
                            }
                        }
                    }
                    Err(e) => {
                        println!("Error: {:?}", e);
                    }
                }
            }
        }
    }
}
