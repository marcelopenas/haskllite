fn void() -> () {
    println!("unity test");
    return ();
}

let mut b:i32 = 5; // Global variable

fn sum(x:i32, y:i32) ->  i32 {
    println!("hello");
    let mut a:i32; // Local
    a = x + y; 
    println!(a); // Prints 7
    println!("bye");
    return a;
}

fn main() {
    let mut x: i32 = 1;
    println!(x);
    while (x < 5) {
        println!(x);
        x = x + 1;
    }
    println!(x); // Prints 5
    println!(sum(1,2)); // Prints 1+2 twice (one inside the function one here)
    void();
}
