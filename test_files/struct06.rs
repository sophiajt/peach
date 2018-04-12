struct Bar {
    x: u64,
}

fn main() {
    let bar = Bar { x: 3 };

    bar.x = bar.x + 2;

    println!("{}", bar.x);
}
