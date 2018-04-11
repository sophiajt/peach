fn main() {
    let x = 3;
    {
        x();
    }
    fn x() {
        println!("{}", 2);
    }
}
