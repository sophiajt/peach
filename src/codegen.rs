use bytecode::{Bytecode, Ty};
use std::collections::HashMap;
use time::PreciseTime;

fn codegen_type(ty: &Ty) -> String {
    let codegen_ty = match ty {
        Ty::U64 => "unsigned long long ".into(),
        Ty::Void => "void ".into(),
        Ty::Bool => "bool ".into(),
        _ => unimplemented!("Can't codegen type: {:?}", ty),
    };
    codegen_ty
}

pub fn codegen_bytecode(fn_name: &str, return_type: &Ty, bytecode: &Vec<Bytecode>) {
    let mut output = String::new();

    let mut var_lookup: HashMap<usize, usize> = HashMap::new();

    output += "#include <stdio.h>\n";
    output += "#include <stdbool.h>\n";

    output += &codegen_type(return_type);

    output += fn_name;
    output += "() {\n";

    let mut next_id = 0;
    //TODO: do we need the types here if we can just use 'auto' when we're not sure?
    let mut var_name_stack: Vec<(usize, Ty)> = vec![];

    for bc in bytecode {
        match bc {
            Bytecode::ReturnVoid => {
                output += "return;\n";
                break;
            }
            Bytecode::ReturnLastStackValue => {
                let (retval, _) = var_name_stack.pop().expect("Add needs a rhs in codegen");
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
        }
    }

    output += "};\n";

    output += "int main() { printf(\"output: %llu\", expr()); }";

    let path = {
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::Path;

        let dir = ::std::env::temp_dir();
        let path = Path::new(&dir).join("madness.c");
        let mut file =
            File::create(path.clone()).expect("Can not create temporary .c file for output");
        file.write_all(&output.as_bytes())
            .expect("Failed to write output to .c file");
        path
    };

    {
        let start = PreciseTime::now();
        println!("path: {:?}", path);
        use std::process::Command;
        let output = Command::new(r"C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Tools\MSVC\14.13.26128\bin\Hostx64\x64\cl.exe")
            //.arg("/Ox")
            .arg(path)
            .output()
            .expect("failed to execute compiler");
        let end = PreciseTime::now();
        let duration = start
            .to(end)
            .to_std()
            .expect("Can't convert duration to std duration");

        println!(
            "status: {} in {:.3} sec",
            output.status,
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        );
        println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    }
}
