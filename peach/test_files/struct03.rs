struct Bar {
    x: u64,
    k: u64,
}

fn main() {
    let bar = Bar { x: 3, k: 5 };

    println!("{}", bar.k);
}
