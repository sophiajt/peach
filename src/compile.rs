use bytecode::{Bytecode, BytecodeEngine, Fun, Ty};
//use std::collections::HashMap;
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
    let mut output = String::new();

    //TODO: we may want to add types in the future
    //let mut expression_stack: Vec<(String, Ty)> = vec![];
    let mut expression_stack: Vec<String> = vec![];

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
    }

    for code in &fun.bytecode {
        match code {
            Bytecode::ReturnVoid => {
                output += "return;\n";
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let retval = expression_stack.pop().unwrap();
                output += &format!("return {};\n", retval);
                break;
            }
            Bytecode::PushU64(val) => {
                expression_stack.push(val.to_string());
            }
            Bytecode::PushBool(val) => {
                expression_stack.push(val.to_string());
            }
            Bytecode::Add => {
                let rhs = expression_stack.pop().unwrap();
                let lhs = expression_stack.pop().unwrap();

                expression_stack.push(format!("({}+{})", lhs, rhs));
            }
            Bytecode::Sub => {
                let rhs = expression_stack.pop().unwrap();
                let lhs = expression_stack.pop().unwrap();

                expression_stack.push(format!("({}-{})", lhs, rhs));
            }
            Bytecode::Mul => {
                let rhs = expression_stack.pop().unwrap();
                let lhs = expression_stack.pop().unwrap();

                expression_stack.push(format!("({}*{})", lhs, rhs));
            }
            Bytecode::Div => {
                let rhs = expression_stack.pop().unwrap();
                let lhs = expression_stack.pop().unwrap();

                expression_stack.push(format!("({}/{})", lhs, rhs));
            }
            Bytecode::Lt => {
                let rhs = expression_stack.pop().unwrap();
                let lhs = expression_stack.pop().unwrap();

                expression_stack.push(format!("({} < {})", lhs, rhs));
            }
            Bytecode::VarDecl(var_id) => {
                let rhs = expression_stack.pop().unwrap();

                output += &format!("auto v{} = {};\n", *var_id, rhs);
            }
            Bytecode::Var(var_id) => {
                expression_stack.push(format!("v{}", var_id));
            }
            Bytecode::Assign(var_id) => {
                let rhs = expression_stack.pop().unwrap();

                output += &format!("v{} = {};\n", *var_id, rhs);
            }
            Bytecode::Call(fn_name) => {
                let fun = bc.get_fn(fn_name);
                let mut expr_string = String::new();

                expr_string += &format!("{}(", fn_name);
                let expression_stack_len = expression_stack.len();
                let mut offset = fun.params.len();
                while offset > 0 {
                    expr_string += &expression_stack[expression_stack_len - offset];
                    if offset > 1 {
                        expr_string += ", "
                    }
                    offset -= 1;
                }

                expr_string += ")";
                expression_stack.push(expr_string);
            }
            Bytecode::If(_) => {
                //TODO: fix to expression-friendly for C
                let cond = expression_stack.pop().unwrap();

                output += &format!("if ({}) {{\n", cond);
            }
            Bytecode::EndIf => {
                output += "}\n";
            }
            Bytecode::BeginWhile => {
                output += "while(1) {\n";
            }
            Bytecode::WhileCond(_) => {
                let cond = expression_stack.pop().unwrap();
                output += &format!("if (!({})) break;\n", cond);
            }
            Bytecode::EndWhile(_) => {
                output += "}\n";
            }
            Bytecode::DebugPrint => {
                let val = expression_stack.pop().unwrap();
                output += &format!("printf(\"DEBUG: %u\\n\", ({}));\n", val);
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
    println!("{}", output);

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
