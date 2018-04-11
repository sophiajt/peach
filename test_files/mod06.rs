mod foo {
    pub mod bar {
        pub fn baz() {
            println!("{}", 3);
        }
    }
}

fn main() {
    use foo::bar::baz;

    baz();
}
