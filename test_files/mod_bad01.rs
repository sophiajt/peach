mod foo {
    pub fn bar() {
        baz::qux();
    }
}

mod baz {
    pub fn qux() {
        __debug__(3);
    }
}

fn main() {
    foo::bar();
}
