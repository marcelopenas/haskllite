// This demonstrates scope and shadowing of variables. The inner block can access and modify the outer variable, but it can also declare a new variable with the same name that shadows the outer variable.

let mut x:i32 = 5;

println!(x); // Prints 5

{
  x = 42; // changes outer x to 42
  println!(5); // Prints 5
  let mut x: i32 = 3;
  println!(x); // Prints 3
  x = 2; // assigns inner x to 3, outer is also x but they are different variables
  println!(x); // Prints 2
  x = 1;
  println!(x); // Prints 1
}

println!(x); // Prints 42
