fn print(x: i32) -> str {
  println!("received:");
  println!(x);
  println!("printing:");
  for (i=1; i < 10; i = i+1) {
    println!(i);
    if (i == x) {
      return 42;
    }
  }
  println!("should not print this");
}

let result: i32 = print(3);
println!("result:");
println!(result);
