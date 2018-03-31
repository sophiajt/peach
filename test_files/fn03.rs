fn bar(x: u64, y: u64) -> u64 {
    x - y
}

fn main() {
    __debug__(bar(5, 3))
}
