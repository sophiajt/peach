fn main() {
    {
        x();
    }
    let x = 3;
    fn x() {
        println!("{}", 2);
    }
}
