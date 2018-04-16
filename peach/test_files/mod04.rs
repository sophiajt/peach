mod foo {
    mod other {
        pub fn baz() {
            println!("{}", 4);
        }
    }
    mod bar {
        pub fn baz() {
            println!("{}", 3);
        }
    }
}

fn main() {
    foo::other::baz();
}
