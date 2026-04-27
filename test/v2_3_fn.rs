// fn print(x: i32) -> i32 {
//   println!("received:");
//   println!(x);
//   println!("printing:");
//   for (i=1; i < 10; i = i+1) {
//     println!(i);
//     if (i == x) {
//       return 42;
//     }
//   }
//   println!("should not print this");
// }

fn void() -> () {
  println!("unity test");
  return ();
}

// let result: i32 = print(3);
// println!("result:");
// println!(result);

let result2: i32 = void();
println!("result2:");
println!(result2);


