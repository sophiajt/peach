mod foo {
    pub mod bar {
        pub fn baz() {
            __debug__(3);
        }
    }
}

fn main() {
    use foo::bar::baz;

    baz();
}
