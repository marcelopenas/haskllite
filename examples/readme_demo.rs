fn main() { // This is the demo from the readme
    println!("Haskllite");
    let mut x: i32 = 10;
    while (x > 0) {
        println!(x);
        x = x - 1;
    }
    let y: f64 = 4.2;
    let mut z: str = (str) y; // Casting to string

    let mut text: str = (str) scanln!();

    println!((str) z + text);
} // This works only on interpreted mode
