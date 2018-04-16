use std::env;

fn main() {
    let target = env::var("TARGET").unwrap();
    let target_os = target.as_str().splitn(3, "-").nth(2).unwrap();

    if target_os.contains("windows") {
        println!("cargo:rustc-link-lib=static=SDL2main");
        println!("cargo:rustc-link-lib=static=SDL2");
    } else {
        println!("cargo:rustc-link-lib=dylib=SDL2main");
        println!("cargo:rustc-link-lib=dylib=SDL2");
    }
}
