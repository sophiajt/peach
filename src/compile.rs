use bytecode::{Bytecode, BytecodeEngine, Fun, Ty};
use std::collections::HashMap;
use time::PreciseTime;

fn codegen_type(ty: &Ty) -> String {
    let codegen_ty = match ty {
        Ty::U64 => "unsigned long long".into(),
        Ty::Void => "void".into(),
        Ty::Bool => "bool".into(),
        _ => unimplemented!("Can't codegen type: {:?}", ty),
    };
    codegen_ty
}

fn codegen_fn_header(fn_name: &str, fun: &Fun) -> String {
    let mut output = String::new();
    //TODO: tighten this up with a format!
    output += &codegen_type(&fun.return_ty);
    output += " ";
    output += fn_name;
    output += "();\n";

    output
}

fn codegen_fn(bc: &BytecodeEngine, fn_name: &str, fun: &Fun) -> String {
    let mut var_lookup: HashMap<usize, usize> = HashMap::new();
    let mut output = String::new();

    //TODO: do we need the types here if we can just use 'auto' when we're not sure?
    let mut var_name_stack: Vec<(usize, Ty)> = vec![];

    output += &codegen_type(&fun.return_ty);
    output += " ";
    output += fn_name;
    output += "(";

    let mut first = true;
    for param in &fun.params {
        output += &format!(
            "{}{} {}",
            if !first { ", " } else { "" },
            codegen_type(&param.ty),
            param.name
        );
        first = false;
    }

    output += ") {\n";
    for param in &fun.params {
        output += &format!(
            "{} v{} = {};\n",
            codegen_type(&param.ty),
            param.var_id,
            param.name
        );
        var_name_stack.push((param.var_id, param.ty.clone()));
        var_lookup.insert(param.var_id, var_name_stack.len() - 1);
    }
    let mut next_id = fun.params.len();

    for code in &fun.bytecode {
        match code {
            Bytecode::ReturnVoid => {
                output += "return;\n";
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let (retval, _) = var_name_stack
                    .pop()
                    .expect("Add needs a value to return in codegen");
                output += &format!("return v{};\n", retval);
                break;
            }
            Bytecode::PushU64(val) => {
                output += &format!("unsigned long long v{} = {};\n", next_id, val);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::PushBool(val) => {
                output += &format!("bool v{} = {};\n", next_id, val);
                var_name_stack.push((next_id, Ty::Bool));
                next_id += 1;
            }
            Bytecode::Add => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}+v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Sub => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}-v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Mul => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}*v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::Div => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let (lhs, _) = var_name_stack.pop().expect("Add needs a lhs in codegen");
                output += &format!("unsigned long long v{} = v{}/v{};\n", next_id, lhs, rhs);
                var_name_stack.push((next_id, Ty::U64));
                next_id += 1;
            }
            Bytecode::VarDecl(var_id) => {
                let last_pos = var_name_stack.len() - 1;
                var_lookup.insert(*var_id, last_pos);
            }
            Bytecode::Var(var_id) => {
                let id = var_lookup[var_id];
                // TODO: we need a better way to output the type name
                let (ref var, ref ty) = var_name_stack[id];

                output += &format!("{} v{} = v{};\n", codegen_type(ty), next_id, var);

                var_name_stack.push((next_id, ty.clone()));
                next_id += 1;
            }
            Bytecode::Assign(var_id) => {
                let (rhs, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
                let id = var_lookup[var_id];
                // TODO: we need a better way to output the type name
                let (ref var, _) = var_name_stack[id];

                output += &format!("v{} = v{};\n", var, rhs);
            }
            Bytecode::Call(fn_name) => {
                let fun = bc.get_fn(fn_name);
                output += &format!(
                    "{} v{} = {}(",
                    codegen_type(&fun.return_ty),
                    next_id,
                    fn_name
                );

                let mut offset = fun.params.len();
                let var_name_stack_len = var_name_stack.len();
                while offset > 0 {
                    output += &format!("v{}", var_name_stack[var_name_stack_len - offset].0);
                    if offset > 1 {
                        output += ", "
                    }
                    offset -= 1;
                }

                output += ");\n";
                var_name_stack.push((next_id, fun.return_ty.clone()));
                next_id += 1;
            }
            Bytecode::DebugPrint => {
                let (debug_id, debug_ty) = var_name_stack
                    .pop()
                    .expect("Add needs a value to debug print in codegen");

                output += &format!(
                    "printf(\"DEBUG: {}\\n\", v{});\n",
                    match debug_ty {
                        Ty::Bool => "%s",
                        Ty::U64 => "%llu",
                        _ => unimplemented!("Can't debug print values of type {:?}", debug_ty),
                    },
                    match debug_ty {
                        Ty::Bool => format!("{}?\"true\":\"false\"", debug_id),
                        _ => format!("{}", debug_id),
                    }
                );
            }
        }
    }

    output += "};\n";
    output
}

fn codegen_c_from_bytecode(bc: &BytecodeEngine) -> String {
    let mut output = String::new();

    output += "#include <stdio.h>\n";
    output += "#include <stdbool.h>\n";

    for fn_name in bc.processed_fns.keys() {
        let fun = bc.get_fn(fn_name);
        output += &codegen_fn_header(fn_name, fun);
    }
    for fn_name in bc.processed_fns.keys() {
        let fun = bc.get_fn(fn_name);
        output += &codegen_fn(bc, fn_name, fun);
    }

    output
}

pub fn compile_bytecode(bc: &BytecodeEngine, input_fname: &str) -> ::std::io::Result<String> {
    let output = codegen_c_from_bytecode(bc);
    //println!("{}", output);

    let path = {
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::Path;

        let input_path = Path::new(input_fname);

        let dir = ::std::env::temp_dir();
        let path = Path::new(&dir)
            .join(input_path.file_name().unwrap())
            .with_extension("c");
        let mut file =
            File::create(path.clone()).expect(&format!("Can not create {:?} for output", path));
        file.write_all(&output.as_bytes())
            .expect("Failed to write output to .c file");
        path
    };

    compile_file(path)
}

#[cfg(windows)]
fn compile_file(path: ::std::path::PathBuf) -> ::std::io::Result<String> {
    let start = PreciseTime::now();
    //println!("path: {:?}", path);
    use std::process::Command;

    let output_fname = String::new() + path.with_extension("exe").to_str().unwrap();
    let output_objname = String::new() + path.with_extension("obj").to_str().unwrap();

    let output = Command::new(r"cl.exe")
        .arg(&format!("/Fe{}", output_fname))
        .arg(&format!("/Fo{}", output_objname))
        .arg(path)
        .output()?;
    let end = PreciseTime::now();
    let duration = start
        .to(end)
        .to_std()
        .expect("Can't convert duration to std duration");

    println!(
        "status: {} in {:.3} sec",
        if output.status.success() {
            "success"
        } else {
            "fail"
        },
        duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
    );

    if output.status.success() {
        Ok(output_fname)
    } else {
        use std::io::{Error, ErrorKind};

        let compile_stdout = String::from_utf8(output.stdout).unwrap();
        let compile_stderr = String::from_utf8(output.stderr).unwrap();

        let combined_compile_msg = compile_stdout + &compile_stderr;

        Err(Error::new(ErrorKind::Other, combined_compile_msg))
    }
}

#[cfg(unix)]
fn compile_file(path: ::std::path::PathBuf) -> ::std::io::Result<String> {
    let start = PreciseTime::now();
    println!("path: {:?}", path);
    use std::process::Command;
    let output_fname = String::new() + path.with_extension("").to_str().unwrap();

    let output = Command::new(r"clang")
        .arg(path)
        .arg("-o")
        .arg(&output_fname)
        .output()
        .expect("failed to execute compiler");
    let end = PreciseTime::now();
    let duration = start
        .to(end)
        .to_std()
        .expect("Can't convert duration to std duration");

    println!(
        "status: {} in {:.3} sec",
        if output.status.success() {
            "success"
        } else {
            "fail"
        },
        duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
    );

    if output.status.success() {
        Ok(output_fname)
    } else {
        use std::io::{Error, ErrorKind};

        let compile_stdout = String::from_utf8(output.stdout).unwrap();
        let compile_stderr = String::from_utf8(output.stderr).unwrap();

        let combined_compile_msg = compile_stdout + &compile_stderr;

        Err(Error::new(ErrorKind::Other, combined_compile_msg))
    }
}
