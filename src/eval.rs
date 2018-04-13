use bytecode::{Bytecode, BytecodeEngine, Definition, Fun, Processed};
use std::collections::HashMap;
use std::fmt;
use typecheck::{builtin_type, TypeInfo};

#[derive(Debug, Clone)]
pub enum Value {
    U64(u64),
    U32(u32),
    I64(i64),
    I32(i32),
    UnknownInt(i32),
    Bool(bool),
    Error,
    Void,
    Object(HashMap<String, usize>),
    Reference(usize), // reference into the value stack
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::U64(x) => x.to_string(),
                Value::U32(x) => x.to_string(),
                Value::I64(x) => x.to_string(),
                Value::I32(x) => x.to_string(),
                Value::UnknownInt(x) => x.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Error => "error".to_string(),
                Value::Void => "void".to_string(),
                Value::Object(dict) => format!("object: {:?}", dict),
                Value::Reference(pos) => format!("reference: {}", pos),
            }
        )
    }
}

pub fn eval_block_bytecode(
    bc: &BytecodeEngine,
    bytecode: &Vec<Bytecode>,
    var_lookup: &mut HashMap<usize, usize>,
    value_stack: &mut Vec<Value>,
    debug_capture: &mut Option<String>,
) -> Value {
    let bytecode_len = bytecode.len();
    let mut idx = 0;
    while idx < bytecode_len {
        let code = &bytecode[idx];
        match code {
            Bytecode::ReturnVoid => {
                return Value::Void;
            }
            Bytecode::ReturnLastStackValue => match value_stack.pop() {
                Some(s) => return s,
                _ => return Value::Error,
            },
            Bytecode::As(type_id) => match value_stack.pop() {
                Some(Value::UnknownInt(val)) => match *type_id {
                    builtin_type::I32 => {
                        value_stack.push(Value::I32(val as i32));
                    }
                    builtin_type::I64 => {
                        value_stack.push(Value::I64(val as i64));
                    }
                    builtin_type::U32 => {
                        value_stack.push(Value::U32(val as u32));
                    }
                    builtin_type::U64 => {
                        value_stack.push(Value::U64(val as u64));
                    }
                    _ => {
                        unimplemented!("Trying to convert {{unknown int}} to non-integer type");
                    }
                },
                Some(x) => value_stack.push(x),
                None => unimplemented!("Can not do a type conversion of missing value"),
            },
            Bytecode::Neg => match value_stack.pop() {
                Some(Value::I64(val)) => {
                    value_stack.push(Value::I64(-val));
                }
                Some(Value::I32(val)) => {
                    value_stack.push(Value::I32(-val));
                }
                Some(Value::UnknownInt(val)) => {
                    value_stack.push(Value::UnknownInt(-val));
                }
                x => unimplemented!("Can't negate values of {:?}", x),
            },
            Bytecode::Add => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs + rhs));
                }
                (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                    value_stack.push(Value::U32(lhs + rhs));
                }
                (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                    value_stack.push(Value::I64(lhs + rhs));
                }
                (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                    value_stack.push(Value::I32(lhs + rhs));
                }
                (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                    value_stack.push(Value::UnknownInt(lhs + rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Sub => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs - rhs));
                }
                (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                    value_stack.push(Value::U32(lhs - rhs));
                }
                (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                    value_stack.push(Value::I64(lhs - rhs));
                }
                (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                    value_stack.push(Value::I32(lhs - rhs));
                }
                (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                    value_stack.push(Value::UnknownInt(lhs - rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Mul => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs * rhs));
                }
                (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                    value_stack.push(Value::U32(lhs * rhs));
                }
                (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                    value_stack.push(Value::I64(lhs * rhs));
                }
                (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                    value_stack.push(Value::I32(lhs * rhs));
                }
                (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                    value_stack.push(Value::UnknownInt(lhs * rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Div => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::U64(lhs / rhs));
                }
                (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                    value_stack.push(Value::U32(lhs / rhs));
                }
                (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                    value_stack.push(Value::I64(lhs / rhs));
                }
                (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                    value_stack.push(Value::I32(lhs / rhs));
                }
                (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                    value_stack.push(Value::UnknownInt(lhs / rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Lt => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                    value_stack.push(Value::Bool(lhs < rhs));
                }
                (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
            },
            Bytecode::Dot(field) => match value_stack.pop() {
                Some(Value::Object(obj)) => {
                    if obj.contains_key(field) {
                        value_stack.push(value_stack[obj[field]].clone())
                    } else {
                        unimplemented!("Can not find field {} in object {:#?}", field, obj);
                    }
                }
                _ => {
                    unimplemented!("Dot access on value that isn't an object");
                }
            },
            Bytecode::LValueDot(field) => match value_stack.pop() {
                Some(Value::Reference(slot)) => match value_stack[slot] {
                    Value::Object(ref obj) => {
                        if obj.contains_key(field) {
                            value_stack.push(Value::Reference(obj[field]))
                        } else {

                        }
                    }
                    _ => unimplemented!("Field access of non-object"),
                },
                _ => {
                    unimplemented!("Field access into unknown value");
                }
            },
            Bytecode::PushU64(val) => {
                value_stack.push(Value::U64(*val));
            }
            Bytecode::PushU32(val) => {
                value_stack.push(Value::U32(*val));
            }
            Bytecode::PushI64(val) => {
                value_stack.push(Value::I64(*val));
            }
            Bytecode::PushI32(val) => {
                value_stack.push(Value::I32(*val));
            }
            Bytecode::PushUnknownInt(val) => {
                value_stack.push(Value::UnknownInt(*val));
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
            Bytecode::VarDeclUninit(var_id) => {
                //push a dummy value on the stack to give us a slot for a value later
                value_stack.push(Value::Void);
                var_lookup.insert(*var_id, value_stack.len() - 1);
            }
            Bytecode::Var(var_id) => {
                let pos: usize = var_lookup[var_id];
                value_stack.push(value_stack[pos].clone());
            }
            Bytecode::LValueVar(var_id) => {
                let pos: usize = var_lookup[var_id];
                value_stack.push(Value::Reference(pos));
            }
            Bytecode::Assign => match (value_stack.pop(), value_stack.pop()) {
                (Some(Value::Reference(slot)), Some(rhs)) => {
                    value_stack[slot] = rhs;
                }
                _ => unimplemented!("Assignment missing right-hand side value"),
            },
            Bytecode::Call(definition_id) => {
                if let Definition::Processed(Processed::Fun(ref target_fun)) =
                    bc.definitions[*definition_id]
                {
                    let result = eval_fn_bytecode(bc, target_fun, value_stack, debug_capture);
                    value_stack.push(result);
                } else if let Definition::Processed(Processed::Struct(ref s)) =
                    bc.definitions[*definition_id]
                {
                    if let TypeInfo::Struct(ref st) = bc.typechecker.types[s.type_id] {
                        let mut hash = HashMap::new();
                        let mut offset = 1;
                        for field in st.fields.iter().rev() {
                            hash.insert(field.0.clone(), value_stack.len() - offset);
                            offset += 1;
                        }
                        value_stack.push(Value::Object(hash))
                    } else {
                        unimplemented!("Can not find struct type for object");
                    }
                } else {
                    unimplemented!("Eval of unprocessed function");
                }
            }
            Bytecode::DebugPrint(_) => match value_stack.pop() {
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

    eval_block_bytecode(
        bc,
        &fun.bytecode,
        &mut var_lookup,
        value_stack,
        debug_capture,
    )
}

/// Begin evaluating the bytecode starting at the given function name.  Optionally, capture the debug output for later use.
pub fn eval_engine(
    bc: &BytecodeEngine,
    starting_fn_name: &str,
    debug_capture: &mut Option<String>,
) -> Value {
    // begin evaluating with the first function
    // We assume scope 0 is the file root scope of the starting file, where will find the main
    let fun = bc.get_fn(starting_fn_name, 0);
    let mut value_stack: Vec<Value> = vec![];

    eval_fn_bytecode(bc, &fun, &mut value_stack, debug_capture)
}
