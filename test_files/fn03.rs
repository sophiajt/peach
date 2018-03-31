fn bar(x: u64, y: u64) -> u64 {
    x - y
}

fn expr() -> u64 {
    bar(5, 3)
}
