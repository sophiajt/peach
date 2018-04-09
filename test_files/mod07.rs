mod foo {
    pub mod bar {
        pub fn baz() -> u64 {
            3
        }

        pub fn bazz() -> u64 {
            4
        }
    }
}

fn main() {
    use foo::bar::{baz, bazz};

    __debug__(bazz() - baz());
}
