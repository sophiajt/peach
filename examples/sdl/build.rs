fn main() {
    println!("cargo:rustc-link-lib=static=SDL2main");
    println!("cargo:rustc-link-lib=static=SDL2");
}
