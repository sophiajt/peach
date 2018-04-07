mod foo {
    mod bar {
        pub fn baz() {
            __debug__(2);
        }
    }
}

fn main() {
    foo::bar::baz();
}
