use bytecode::{Bytecode, BytecodeEngine, Fun, Ty};
use time::PreciseTime;

struct CFile {
    output_src: String,
    expression_stack: Vec<String>,
}

impl CFile {
    fn new() -> CFile {
        CFile {
            output_src: String::new(),
            expression_stack: vec![],
        }
    }

    fn codegen_raw(&mut self, output: &str) {
        self.output_src += output;
    }

    fn delay_expr(&mut self, expr: String) {
        self.expression_stack.push(expr);
    }

    fn codegen_stmt(&mut self, stmt: &str) {
        for expr in self.expression_stack.drain(0..) {
            self.output_src += &expr;
            self.output_src += ";\n";
        }

        self.output_src += stmt;
    }
}

fn codegen_type(ty: &Ty) -> String {
    let codegen_ty = match ty {
        Ty::U64 => "unsigned long long".into(),
        Ty::U32 => "unsigned".into(),
        Ty::UnknownInt => "int".into(),
        Ty::Void => "void".into(),
        Ty::Bool => "bool".into(),
        _ => unimplemented!("Can't codegen type: {:?}", ty),
    };
    codegen_ty
}

fn codegen_fn_header(fn_name: &str, fun: &Fun) -> String {
    format!("{} {}();\n", codegen_type(&fun.return_ty), fn_name)
}

fn codegen_fn(cfile: &mut CFile, bc: &BytecodeEngine, fn_name: &str, fun: &Fun) {
    let mut next_temp_id = 0;

    //TODO: This isn't the best solution, but it's an experiment
    let mut temp_id_stack = vec![];

    cfile.codegen_raw(&format!("{} {}(", &codegen_type(&fun.return_ty), fn_name));

    let mut first = true;
    for param in &fun.params {
        cfile.codegen_raw(&format!(
            "{}{} {}",
            if !first { ", " } else { "" },
            codegen_type(&param.ty),
            param.name
        ));
        first = false;
    }

    cfile.codegen_raw(") {\n");

    for param in &fun.params {
        cfile.codegen_raw(&format!(
            "{} v{} = {};\n",
            codegen_type(&param.ty),
            param.var_id,
            param.name
        ));
    }

    for code in &fun.bytecode {
        match code {
            Bytecode::ReturnVoid => {
                cfile.codegen_stmt("return;\n");
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let retval = cfile.expression_stack.pop().unwrap();
                cfile.codegen_stmt(&format!("return {};\n", retval));
                break;
            }
            Bytecode::PushU64(val) => {
                cfile.delay_expr(val.to_string());
            }
            Bytecode::PushU32(val) => {
                cfile.delay_expr(val.to_string());
            }
            Bytecode::PushBool(val) => {
                cfile.delay_expr(val.to_string());
            }
            Bytecode::Add => {
                let rhs = cfile.expression_stack.pop().unwrap();
                let lhs = cfile.expression_stack.pop().unwrap();

                cfile.delay_expr(format!("({}+{})", lhs, rhs));
            }
            Bytecode::Sub => {
                let rhs = cfile.expression_stack.pop().unwrap();
                let lhs = cfile.expression_stack.pop().unwrap();

                cfile.delay_expr(format!("({}-{})", lhs, rhs));
            }
            Bytecode::Mul => {
                let rhs = cfile.expression_stack.pop().unwrap();
                let lhs = cfile.expression_stack.pop().unwrap();

                cfile.delay_expr(format!("({}*{})", lhs, rhs));
            }
            Bytecode::Div => {
                let rhs = cfile.expression_stack.pop().unwrap();
                let lhs = cfile.expression_stack.pop().unwrap();

                cfile.delay_expr(format!("({}/{})", lhs, rhs));
            }
            Bytecode::Lt => {
                let rhs = cfile.expression_stack.pop().unwrap();
                let lhs = cfile.expression_stack.pop().unwrap();

                cfile.delay_expr(format!("({} < {})", lhs, rhs));
            }
            Bytecode::VarDecl(var_id) => {
                let var = &fun.vars[*var_id];
                let rhs = cfile.expression_stack.pop().unwrap();

                cfile.codegen_stmt(&format!(
                    "{} v{} = {};\n",
                    codegen_type(&var.ty),
                    *var_id,
                    rhs
                ));
            }
            Bytecode::VarDeclUninit(var_id) => {
                let var = &fun.vars[*var_id];

                cfile.codegen_stmt(&format!("{} v{};\n", codegen_type(&var.ty), *var_id));
            }
            Bytecode::Var(var_id) => {
                cfile.delay_expr(format!("v{}", var_id));
            }
            Bytecode::Assign(var_id) => {
                let rhs = cfile.expression_stack.pop().unwrap();

                cfile.codegen_stmt(&format!("v{} = {};\n", *var_id, rhs));
            }
            Bytecode::Call(fn_name) => {
                let fun = bc.get_fn(fn_name);
                let mut expr_string = String::new();

                expr_string += &format!("{}(", fn_name);
                let expression_stack_len = cfile.expression_stack.len();
                let mut offset = fun.params.len();
                while offset > 0 {
                    expr_string += &cfile.expression_stack[expression_stack_len - offset];
                    if offset > 1 {
                        expr_string += ", "
                    }
                    offset -= 1;
                }

                expr_string += ")";
                cfile.delay_expr(expr_string);
            }
            Bytecode::If(_, ty) => {
                //TODO using codegen_raw here is probably not correct, but if we don't we run
                //into issues with if expressions

                let cond = cfile.expression_stack.pop().unwrap();

                match ty {
                    Ty::U64 | Ty::Bool | Ty::U32 | Ty::UnknownInt => {
                        cfile.codegen_raw(&format!("{} t{};\n", codegen_type(ty), next_temp_id));
                        temp_id_stack.push(next_temp_id);
                        next_temp_id += 1;
                    }
                    _ => {}
                }

                cfile.codegen_raw(&format!("if ({}) {{\n", cond));
            }
            Bytecode::Else(_, ty) => {
                if *ty != Ty::Void {
                    let result = cfile.expression_stack.pop().unwrap();
                    cfile.codegen_raw(&format!(
                        "t{} = {};\n",
                        temp_id_stack.last().unwrap(),
                        result
                    ));
                }
                cfile.codegen_raw("} else {\n");
            }
            Bytecode::EndIf(ty) => {
                if *ty != Ty::Void {
                    let result = cfile.expression_stack.pop().unwrap();
                    let temp_id = temp_id_stack.pop().unwrap();
                    cfile.codegen_raw(&format!("t{} = {};\n", temp_id, result));
                    cfile.expression_stack.push(format!("t{}", temp_id));
                }
                cfile.codegen_raw("}\n");
            }
            Bytecode::BeginWhile => {
                cfile.codegen_stmt("while(1) {\n");
            }
            Bytecode::WhileCond(_) => {
                let cond = cfile.expression_stack.pop().unwrap();
                cfile.codegen_stmt(&format!("if (!({})) break;\n", cond));
            }
            Bytecode::EndWhile(_) => {
                cfile.codegen_stmt("}\n");
            }
            Bytecode::DebugPrint => {
                let val = cfile.expression_stack.pop().unwrap();
                cfile.codegen_stmt(&format!("printf(\"DEBUG: %u\\n\", ({}));\n", val));
            }
        }
    }

    cfile.codegen_stmt("};\n");
}

fn codegen_c_from_bytecode(bc: &BytecodeEngine) -> String {
    let mut cfile = CFile::new();

    cfile.codegen_raw("#include <stdio.h>\n");
    cfile.codegen_raw("#include <stdbool.h>\n");

    for fn_name in bc.processed_fns.keys() {
        let fun = bc.get_fn(fn_name);
        cfile.codegen_raw(&codegen_fn_header(fn_name, fun));
    }
    for fn_name in bc.processed_fns.keys() {
        let fun = bc.get_fn(fn_name);
        &codegen_fn(&mut cfile, bc, fn_name, fun);
    }

    cfile.output_src
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
