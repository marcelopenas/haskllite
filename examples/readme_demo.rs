const N = 10;

fn main() { // This is the demo from the readme
    println!("Haskllite");
    let mut x: i32 = if 1 == 1 {N} else {0};
    while (x > 0) {
        println!(x);
        x = x - 1;
    }
    let y: f64 = 4.2;
    let mut z: str = (str) y; // Casting to string

    let mut text: str = (str) scanln!();

    println!((str) z + text);
    //! */ ;;; weird comment()
    let mut result: bool = 42 > 67 ;
    for (i = 0; i < 7; i = i + 1) {
        result = !result;
    }

    println!(result + "... or False?"); // Implicit casting!
} // This works!!
