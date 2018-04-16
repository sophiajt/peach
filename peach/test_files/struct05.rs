struct Bar {
    y: u64,
    x: u64,
}

fn main() {
    let bar = Bar { x: 4, y: 5 };

    println!("{}", bar.x);
}
