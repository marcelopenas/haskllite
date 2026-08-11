fn main() { 
    let x: f64 = (f64) 5;
    println!(x);
    // if mut fist will throw scope error, otherwise will throw incompatible types
    let mut x: i32 = "hello";
    let x: i32 = "hello";
}