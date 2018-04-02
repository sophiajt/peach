use bytecode::{Bytecode, BytecodeEngine, Fun};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    U64(u64),
    Bool(bool),
    Error,
    Void,
}

fn eval_fn_bytecode(
    bc: &BytecodeEngine,
    fun: &Fun,
    value_stack: &mut Vec<Value>,
    debug_capture: &mut Option<String>,
) -> Value {
    let mut var_lookup: HashMap<usize, usize> = HashMap::new();

    let mut param_offset = fun.params.len();
    for param in &fun.params {
        var_lookup.insert(param.var_id, value_stack.len() - param_offset);
        param_offset -= 1;
    }

    let bytecode_len = fun.bytecode.len();
    let mut idx = 0;
    while idx < bytecode_len {
        let code = &fun.bytecode[idx];
        match code {
            Bytecode::ReturnVoid => {
                return Value::Void;
            }
            Bytecode::ReturnLastStackValue => match value_stack.pop() {
                Some(s) => return s,
                _ => return Value::Error,
            },
            Bytecode::Add => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs + rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Sub => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs - rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Mul => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs * rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Div => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs / rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Lt => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::PushU64(val) => {
                value_stack.push(Value::U64(*val));
            }
            Bytecode::PushBool(val) => {
                value_stack.push(Value::Bool(*val));
            }
            Bytecode::If(offset, _) => match value_stack.pop() {
                Some(Value::Bool(cond)) => {
                    if !cond {
                        idx += offset;
                        continue;
                    }
                }
                _ => unimplemented!("Expected boolean condition for if"),
            },
            Bytecode::Else(offset, _) => {
                idx += offset;
                continue;
            }
            Bytecode::EndIf(_) => {}
            Bytecode::BeginWhile => {}
            Bytecode::WhileCond(offset) => match value_stack.pop() {
                Some(Value::Bool(cond)) => {
                    if !cond {
                        idx += offset + 1; // Eval will also want to skip the EndWhile
                        continue;
                    }
                }
                _ => unimplemented!("Expected boolean condition for if"),
            },
            Bytecode::EndWhile(offset) => {
                idx -= offset;
                continue;
            }
            Bytecode::VarDecl(var_id) => {
                var_lookup.insert(*var_id, value_stack.len() - 1);
            }
            Bytecode::Var(var_id) => {
                let pos: usize = var_lookup[var_id];
                value_stack.push(value_stack[pos].clone());
            }
            Bytecode::Assign(var_id) => {
                match value_stack.pop() {
                    Some(val) => {
                        //TODO add type checking here
                        let pos: usize = var_lookup[var_id];
                        value_stack[pos] = val;
                    }
                    _ => unimplemented!("Assignment missing right-hand side value"),
                }
            }
            Bytecode::Call(fn_name) => {
                let target_fun = bc.get_fn(fn_name);
                let result = eval_fn_bytecode(bc, &target_fun, value_stack, debug_capture);
                value_stack.push(result);
            }
            Bytecode::DebugPrint => match value_stack.pop() {
                Some(s) => match debug_capture {
                    Some(ref mut debug_log) => {
                        debug_log.push_str(&format!("DEBUG: {:?}\n", s));
                    }
                    None => {
                        println!("DEBUG: {:?}", s);
                    }
                },
                _ => unimplemented!("Internal error: debug printing missing value"),
            },
        }

        idx += 1;
    }

    Value::Void
}

pub fn eval_engine(
    bc: &BytecodeEngine,
    starting_fn_name: &str,
    debug_capture: &mut Option<String>,
) -> Value {
    // begin evaluating with the first function
    let fun = bc.get_fn(starting_fn_name);
    let mut value_stack: Vec<Value> = vec![];

    eval_fn_bytecode(bc, &fun, &mut value_stack, debug_capture)
}
