mod foo {
    pub mod bar {
        pub fn baz() -> u64 {
            5
        }

        pub fn bazz() -> u64 {
            8
        }
    }
}

fn main() {
    use foo::bar::*;

    __debug__(bazz() - baz());
}
