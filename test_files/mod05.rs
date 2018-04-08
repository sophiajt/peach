mod foo {
    pub fn bar() {
        ::baz::qux();
    }
}

mod baz {
    pub fn qux() {
        __debug__(7);
    }
}

fn main() {
    foo::bar();
}
