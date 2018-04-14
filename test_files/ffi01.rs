extern "C" {
    fn abs(input: i32) -> i32;
}

fn main() {
    let x: i32 = -1;

    unsafe {
        println!("{}", abs(x));
    }
}
