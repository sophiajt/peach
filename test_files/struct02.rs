struct Bar {
    x: u64,
}

fn main() {
    let bar = Bar { x: 3 };

    __debug__(bar.x);
}
