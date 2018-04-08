fn main() {
    {
        x();
    }
    let x = 3;
    fn x() {
        __debug__(2);
    }
}
