struct A {
  let mut b:i32;
};

fn main() ->  (){
  let x:A; // Creates an instance of A
  x.b = 1; // Assigns 1 to x.b
  println!(x.b); // Prints 1
}
