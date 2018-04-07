mod foo {
    mod other {
        pub fn baz() {
            __debug__(4);
        }
    }
    mod bar {
        pub fn baz() {
            __debug__(3);
        }
    }
}

fn main() {
    foo::other::baz();
}
