fn main() {
    let x = 3;
    {
        x();
    }
    fn x() {
        __debug__(2);
    }
}
