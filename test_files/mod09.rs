mod foo {
    pub mod bar {
        pub fn baz() {
            __debug__(4);
        }
    }
}

fn main() {
    use foo::bar::baz as simple;

    simple();
}
