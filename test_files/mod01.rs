mod foo {
    pub fn bar() {
        __debug__(1);
    }
}

fn main() {
    foo::bar();
}
