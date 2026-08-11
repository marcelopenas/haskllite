fn main() {
    println!("Haskllite");
    let mut x: i32 = 10;
    while (x > 0) {
        println!(x);
        x = x - 1;
    }
    let y: f64 = 4.2;
    let mut z: str = (str) y;

    let mut text: str;
    text = (str) scanln!();

    println!((str) z + text);
}
