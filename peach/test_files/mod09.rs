mod foo {
    pub mod bar {
        pub fn baz() {
            println!("{}", 4);
        }
    }
}

fn main() {
    use foo::bar::baz as simple;

    simple();
}
