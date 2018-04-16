mod foo {
    mod bar {
        pub fn baz() {
            println!("{}", 2);
        }
    }
}

fn main() {
    foo::bar::baz();
}
