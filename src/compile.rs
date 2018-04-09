use bytecode::{Bytecode, BytecodeEngine, DefinitionState, Fun, Processed, Ty};
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
                // For checked add
                //cfile.delay_expr(format!("add32({},{})", lhs, rhs));
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
            Bytecode::Call(definition_id) => {
                if let DefinitionState::Processed(Processed::Fun(ref fun)) =
                    bc.definitions[*definition_id]
                {
                    let mut expr_string = String::new();

                    expr_string += &format!("fun_{}(", definition_id);
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
                    for _ in 0..fun.params.len() {
                        cfile.expression_stack.pop();
                    }
                    cfile.delay_expr(expr_string);
                } else {
                    unimplemented!("Attempt to call unprocessed function");
                }
            }
            Bytecode::If(_, ty) => {
                let cond = cfile.expression_stack.pop().unwrap();

                match ty {
                    Ty::U64 | Ty::Bool | Ty::U32 | Ty::UnknownInt => {
                        cfile.codegen_stmt(&format!("{} t{};\n", codegen_type(ty), next_temp_id));
                        temp_id_stack.push(next_temp_id);
                        next_temp_id += 1;
                    }
                    _ => {}
                }

                cfile.codegen_stmt(&format!("if ({}) {{\n", cond));
            }
            Bytecode::Else(_, ty) => {
                if *ty != Ty::Void {
                    let result = cfile.expression_stack.pop().unwrap();
                    cfile.codegen_stmt(&format!(
                        "t{} = {};\n",
                        temp_id_stack.last().unwrap(),
                        result
                    ));
                }
                cfile.codegen_stmt("} else {\n");
            }
            Bytecode::EndIf(ty) => {
                if *ty != Ty::Void {
                    let result = cfile.expression_stack.pop().unwrap();
                    let temp_id = temp_id_stack.pop().unwrap();
                    cfile.codegen_stmt(&format!("t{} = {};\n}}\n", temp_id, result));
                    cfile.expression_stack.push(format!("t{}", temp_id));
                } else {
                    cfile.codegen_stmt("}\n");
                }
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
    /* 
    // If we wanted checked add/sub we can use something like this:
    cfile.codegen_raw("#include <assert.h>\n");

    cfile.codegen_raw("int add32(int lhs, int rhs) {\n");
    cfile.codegen_raw("int answer = lhs + rhs;\n");
    cfile.codegen_raw("assert((lhs & 0x8000) == (answer & 0x8000));\n");
    cfile.codegen_raw("return answer;\n");
    cfile.codegen_raw("}\n");

    cfile.codegen_raw("int sub32(int lhs, int rhs) {\n");
    cfile.codegen_raw("int answer = lhs - rhs;\n");
    cfile.codegen_raw("assert((lhs & 0x8000) == (answer & 0x8000));\n");
    cfile.codegen_raw("return answer;\n");
    cfile.codegen_raw("}\n");
    */

    let starting_fn_id = bc.scopes[0].definitions["main"];

    for definition_id in 0..bc.definitions.len() {
        if let DefinitionState::Processed(Processed::Fun(ref fun)) = bc.definitions[definition_id] {
            if definition_id != starting_fn_id {
                cfile.codegen_raw(&codegen_fn_header(&format!("fun_{}", definition_id), fun));
            }
        }
    }

    for definition_id in 0..bc.definitions.len() {
        if let DefinitionState::Processed(Processed::Fun(ref fun)) = bc.definitions[definition_id] {
            if definition_id == starting_fn_id {
                codegen_fn(&mut cfile, bc, "main", fun);
            } else {
                codegen_fn(&mut cfile, bc, &format!("fun_{}", definition_id), fun);
            }
        }
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
        .arg("/w")
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
        .arg("-w")
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
