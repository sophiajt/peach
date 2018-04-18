use bytecode::{builtin_type, Bytecode, BytecodeEngine, Definition, Fun};
use std::any::Any;
use std::collections::HashMap;
use std::fmt;
use std::os::raw::c_void;
use std::ptr;

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
    RawPtr(*const c_void),
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
                Value::RawPtr(_p) => "{raw ptr}".to_string(),
                Value::Reference(pos) => format!("reference: {}", pos),
            }
        )
    }
}

impl Value {
    fn into_box_any(self) -> Box<Any> {
        match self {
            Value::U64(x) => Box::new(x),
            Value::U32(x) => Box::new(x),
            Value::I64(x) => Box::new(x),
            Value::I32(x) => Box::new(x),
            Value::UnknownInt(x) => Box::new(x),
            Value::Bool(x) => Box::new(x),
            Value::RawPtr(x) => Box::new(x),
            _ => unimplemented!("Currently don't support conversion for this type"),
        }
    }

    fn from_box_any(b: Box<Any>) -> Value {
        if let Some(val) = b.downcast_ref::<u64>() {
            Value::U64(*val)
        } else if let Some(val) = b.downcast_ref::<u32>() {
            Value::U32(*val)
        } else if let Some(val) = b.downcast_ref::<i64>() {
            Value::I64(*val)
        } else if let Some(val) = b.downcast_ref::<i32>() {
            Value::I32(*val)
        } else if let Some(val) = b.downcast_ref::<bool>() {
            Value::Bool(*val)
        } else if let Some(val) = b.downcast_ref::<*const c_void>() {
            Value::RawPtr(*val)
        } else if let Some(_) = b.downcast_ref::<()>() {
            Value::Void
        } else {
            unimplemented!("Currently don't support up-conversion for this type")
        }
    }
}

pub struct EvalEngine {
    pub value_stack: Vec<Value>,
    extern_fns: HashMap<String, Box<Fn(&mut Vec<Value>) -> Value>>,
    pub debug_capture: Option<String>,
}

impl EvalEngine {
    pub fn new() -> EvalEngine {
        EvalEngine {
            value_stack: vec![],
            extern_fns: HashMap::new(),
            debug_capture: None,
        }
    }

    pub fn eval_block_bytecode(
        &mut self,
        bc: &BytecodeEngine,
        bytecode: &Vec<Bytecode>,
        var_lookup: &mut HashMap<usize, usize>,
    ) -> Value {
        let bytecode_len = bytecode.len();
        let mut idx = 0;
        while idx < bytecode_len {
            let code = &bytecode[idx];
            match code {
                Bytecode::ReturnVoid => {
                    return Value::Void;
                }
                Bytecode::ReturnLastStackValue => match self.value_stack.pop() {
                    Some(s) => return s,
                    _ => return Value::Error,
                },
                Bytecode::As(type_id) => match self.value_stack.pop() {
                    Some(Value::UnknownInt(val)) => match *type_id {
                        builtin_type::I32 => {
                            self.value_stack.push(Value::I32(val as i32));
                        }
                        builtin_type::I64 => {
                            self.value_stack.push(Value::I64(val as i64));
                        }
                        builtin_type::U32 => {
                            self.value_stack.push(Value::U32(val as u32));
                        }
                        builtin_type::U64 => {
                            self.value_stack.push(Value::U64(val as u64));
                        }
                        _ => {
                            unimplemented!("Trying to convert {{unknown int}} to non-integer type");
                        }
                    },
                    Some(x) => self.value_stack.push(x),
                    None => unimplemented!("Can not do a type conversion of missing value"),
                },
                Bytecode::Neg => match self.value_stack.pop() {
                    Some(Value::I64(val)) => {
                        self.value_stack.push(Value::I64(-val));
                    }
                    Some(Value::I32(val)) => {
                        self.value_stack.push(Value::I32(-val));
                    }
                    Some(Value::UnknownInt(val)) => {
                        self.value_stack.push(Value::UnknownInt(-val));
                    }
                    x => unimplemented!("Can't negate values of {:?}", x),
                },
                Bytecode::Add => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                        self.value_stack.push(Value::U64(lhs + rhs));
                    }
                    (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                        self.value_stack.push(Value::U32(lhs + rhs));
                    }
                    (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                        self.value_stack.push(Value::I64(lhs + rhs));
                    }
                    (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                        self.value_stack.push(Value::I32(lhs + rhs));
                    }
                    (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                        self.value_stack.push(Value::UnknownInt(lhs + rhs));
                    }
                    (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
                },
                Bytecode::Sub => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                        self.value_stack.push(Value::U64(lhs - rhs));
                    }
                    (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                        self.value_stack.push(Value::U32(lhs - rhs));
                    }
                    (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                        self.value_stack.push(Value::I64(lhs - rhs));
                    }
                    (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                        self.value_stack.push(Value::I32(lhs - rhs));
                    }
                    (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                        self.value_stack.push(Value::UnknownInt(lhs - rhs));
                    }
                    (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
                },
                Bytecode::Mul => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                        self.value_stack.push(Value::U64(lhs * rhs));
                    }
                    (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                        self.value_stack.push(Value::U32(lhs * rhs));
                    }
                    (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                        self.value_stack.push(Value::I64(lhs * rhs));
                    }
                    (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                        self.value_stack.push(Value::I32(lhs * rhs));
                    }
                    (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                        self.value_stack.push(Value::UnknownInt(lhs * rhs));
                    }
                    (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
                },
                Bytecode::Div => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                        self.value_stack.push(Value::U64(lhs / rhs));
                    }
                    (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                        self.value_stack.push(Value::U32(lhs / rhs));
                    }
                    (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                        self.value_stack.push(Value::I64(lhs / rhs));
                    }
                    (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                        self.value_stack.push(Value::I32(lhs / rhs));
                    }
                    (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                        self.value_stack.push(Value::UnknownInt(lhs / rhs));
                    }
                    (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
                },
                Bytecode::Lt => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::U64(rhs)), Some(Value::U64(lhs))) => {
                        self.value_stack.push(Value::Bool(lhs < rhs));
                    }
                    (Some(Value::U32(rhs)), Some(Value::U32(lhs))) => {
                        self.value_stack.push(Value::Bool(lhs < rhs));
                    }
                    (Some(Value::I64(rhs)), Some(Value::I64(lhs))) => {
                        self.value_stack.push(Value::Bool(lhs < rhs));
                    }
                    (Some(Value::I32(rhs)), Some(Value::I32(lhs))) => {
                        self.value_stack.push(Value::Bool(lhs < rhs));
                    }
                    (Some(Value::UnknownInt(rhs)), Some(Value::UnknownInt(lhs))) => {
                        self.value_stack.push(Value::Bool(lhs < rhs));
                    }
                    (x, y) => unimplemented!("Can't add values of {:?} and {:?}", x, y),
                },
                Bytecode::Dot(field) => match self.value_stack.pop() {
                    Some(Value::Object(obj)) => {
                        if obj.contains_key(field) {
                            self.value_stack.push(self.value_stack[obj[field]].clone())
                        } else {
                            unimplemented!("Can not find field {} in object {:#?}", field, obj);
                        }
                    }
                    _ => {
                        unimplemented!("Dot access on value that isn't an object");
                    }
                },
                Bytecode::LValueDot(field) => match self.value_stack.pop() {
                    Some(Value::Reference(slot)) => match self.value_stack[slot] {
                        Value::Object(ref obj) => {
                            if obj.contains_key(field) {
                                self.value_stack.push(Value::Reference(obj[field]))
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
                    self.value_stack.push(Value::U64(*val));
                }
                Bytecode::PushU32(val) => {
                    self.value_stack.push(Value::U32(*val));
                }
                Bytecode::PushI64(val) => {
                    self.value_stack.push(Value::I64(*val));
                }
                Bytecode::PushI32(val) => {
                    self.value_stack.push(Value::I32(*val));
                }
                Bytecode::PushUnknownInt(val) => {
                    self.value_stack.push(Value::UnknownInt(*val));
                }
                Bytecode::PushBool(val) => {
                    self.value_stack.push(Value::Bool(*val));
                }
                Bytecode::PushRawPtr(val) => {
                    if val.is_null() {
                        self.value_stack.push(Value::RawPtr(ptr::null()));
                    } else {
                        unimplemented!("Unsupported pointer type")
                    }
                }
                Bytecode::If(offset, _) => match self.value_stack.pop() {
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
                Bytecode::WhileCond(offset) => match self.value_stack.pop() {
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
                    var_lookup.insert(*var_id, self.value_stack.len() - 1);
                }
                Bytecode::VarDeclUninit(var_id) => {
                    //push a dummy value on the stack to give us a slot for a value later
                    self.value_stack.push(Value::Void);
                    var_lookup.insert(*var_id, self.value_stack.len() - 1);
                }
                Bytecode::Var(var_id) => {
                    let pos: usize = var_lookup[var_id];
                    self.value_stack.push(self.value_stack[pos].clone());
                }
                Bytecode::LValueVar(var_id) => {
                    let pos: usize = var_lookup[var_id];
                    self.value_stack.push(Value::Reference(pos));
                }
                Bytecode::Assign => match (self.value_stack.pop(), self.value_stack.pop()) {
                    (Some(Value::Reference(slot)), Some(rhs)) => {
                        self.value_stack[slot] = rhs;
                    }
                    _ => unimplemented!("Assignment missing right-hand side value"),
                },
                Bytecode::Call(definition_id) => {
                    if let Definition::Fun(ref target_fun) = bc.definitions[*definition_id] {
                        if let Some(ref ex_name) = target_fun.extern_name {
                            let result = self.extern_fns[ex_name](&mut self.value_stack);
                            self.value_stack.push(result);
                        } else {
                            let result = self.eval_fn_bytecode(bc, target_fun);
                            self.value_stack.push(result);
                        }
                    } else if let Definition::Struct(ref st) = bc.definitions[*definition_id] {
                        let mut hash = HashMap::new();
                        let mut offset = 1;
                        for field in st.fields.iter().rev() {
                            hash.insert(field.0.clone(), self.value_stack.len() - offset);
                            offset += 1;
                        }
                        self.value_stack.push(Value::Object(hash))
                    } else if let Definition::InstantiatedFun(orig_id, _) =
                        bc.definitions[*definition_id]
                    {
                        if let Definition::Fun(ref target_fun) = bc.definitions[orig_id] {
                            let result = self.eval_fn_bytecode(bc, target_fun);
                            self.value_stack.push(result);
                        }
                    } else {
                        unimplemented!("Eval of unprocessed function");
                    }
                }
                Bytecode::DebugPrint(_) => match self.value_stack.pop() {
                    Some(s) => match self.debug_capture {
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

    fn eval_fn_bytecode(&mut self, bc: &BytecodeEngine, fun: &Fun) -> Value {
        let mut var_lookup: HashMap<usize, usize> = HashMap::new();

        let mut param_offset = fun.params.len();
        for param in &fun.params {
            var_lookup.insert(param.var_id, self.value_stack.len() - param_offset);
            param_offset -= 1;
        }

        self.eval_block_bytecode(bc, &fun.bytecode, &mut var_lookup)
    }

    /// Begin evaluating the bytecode starting at the given function name.  Optionally, capture the debug output for later use.
    pub fn eval_program(&mut self, bc: &BytecodeEngine, starting_fn_name: &str) -> Value {
        // begin evaluating with the first function
        // We assume scope 0 is the file root scope of the starting file, where will find the main

        let fun = bc.get_fn(starting_fn_name, 0);

        self.eval_fn_bytecode(bc, &fun)
    }

    pub fn register_extern_fn_0<Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn() -> Ret,
    ) {
        let fun = Box::new(move |_value_stack: &mut Vec<Value>| -> Value {
            unsafe {
                let result = Box::new(ex_fn());
                Value::from_box_any(result)
            }
        });

        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_1<Arg1: Any, Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match value_stack.pop() {
                Some(val) => unsafe {
                    let arg1 = val.into_box_any().downcast::<Arg1>().unwrap();
                    let result = Box::new(ex_fn(*arg1));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });

        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_2<Arg1: Any, Arg2: Any, Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1, Arg2) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match (value_stack.pop(), value_stack.pop()) {
                (Some(val2), Some(val1)) => unsafe {
                    let arg1 = val1.into_box_any().downcast::<Arg1>().unwrap();
                    let arg2 = val2.into_box_any().downcast::<Arg2>().unwrap();
                    let result = Box::new(ex_fn(*arg1, *arg2));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });

        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_3<Arg1: Any, Arg2: Any, Arg3: Any, Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1, Arg2, Arg3) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match (value_stack.pop(), value_stack.pop(), value_stack.pop()) {
                (Some(val3), Some(val2), Some(val1)) => unsafe {
                    let arg1 = val1.into_box_any().downcast::<Arg1>().unwrap();
                    let arg2 = val2.into_box_any().downcast::<Arg2>().unwrap();
                    let arg3 = val3.into_box_any().downcast::<Arg3>().unwrap();
                    let result = Box::new(ex_fn(*arg1, *arg2, *arg3));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });

        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_4<Arg1: Any, Arg2: Any, Arg3: Any, Arg4: Any, Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1, Arg2, Arg3, Arg4) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match (
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
            ) {
                (Some(val4), Some(val3), Some(val2), Some(val1)) => unsafe {
                    let arg1 = val1.into_box_any().downcast::<Arg1>().unwrap();
                    let arg2 = val2.into_box_any().downcast::<Arg2>().unwrap();
                    let arg3 = val3.into_box_any().downcast::<Arg3>().unwrap();
                    let arg4 = val4.into_box_any().downcast::<Arg4>().unwrap();
                    let result = Box::new(ex_fn(*arg1, *arg2, *arg3, *arg4));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });
        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_5<Arg1: Any, Arg2: Any, Arg3: Any, Arg4: Any, Arg5: Any, Ret: Any>(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1, Arg2, Arg3, Arg4, Arg5) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match (
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
            ) {
                (Some(val5), Some(val4), Some(val3), Some(val2), Some(val1)) => unsafe {
                    let arg1 = val1.into_box_any().downcast::<Arg1>().unwrap();
                    let arg2 = val2.into_box_any().downcast::<Arg2>().unwrap();
                    let arg3 = val3.into_box_any().downcast::<Arg3>().unwrap();
                    let arg4 = val4.into_box_any().downcast::<Arg4>().unwrap();
                    let arg5 = val5.into_box_any().downcast::<Arg5>().unwrap();
                    let result = Box::new(ex_fn(*arg1, *arg2, *arg3, *arg4, *arg5));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });
        self.extern_fns.insert(name.to_string(), fun);
    }

    pub fn register_extern_fn_6<
        Arg1: Any,
        Arg2: Any,
        Arg3: Any,
        Arg4: Any,
        Arg5: Any,
        Arg6: Any,
        Ret: Any,
    >(
        &mut self,
        name: &str,
        ex_fn: unsafe extern "C" fn(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) -> Ret,
    ) {
        let fun = Box::new(move |value_stack: &mut Vec<Value>| -> Value {
            match (
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
                value_stack.pop(),
            ) {
                (Some(val6), Some(val5), Some(val4), Some(val3), Some(val2), Some(val1)) => unsafe {
                    let arg1 = val1.into_box_any().downcast::<Arg1>().unwrap();
                    let arg2 = val2.into_box_any().downcast::<Arg2>().unwrap();
                    let arg3 = val3.into_box_any().downcast::<Arg3>().unwrap();
                    let arg4 = val4.into_box_any().downcast::<Arg4>().unwrap();
                    let arg5 = val5.into_box_any().downcast::<Arg5>().unwrap();
                    let arg6 = val6.into_box_any().downcast::<Arg6>().unwrap();
                    let result = Box::new(ex_fn(*arg1, *arg2, *arg3, *arg4, *arg5, *arg6));
                    Value::from_box_any(result)
                },
                _ => unimplemented!("Can't call function successfully"),
            }
        });

        self.extern_fns.insert(name.to_string(), fun);
    }
}
