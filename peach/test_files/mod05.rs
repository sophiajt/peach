mod foo {
    pub fn bar() {
        ::baz::qux();
    }
}

mod baz {
    pub fn qux() {
        println!("{}", 7);
    }
}

fn main() {
    foo::bar();
}
