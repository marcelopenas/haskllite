let mut b:i32 = 5; // Variável Global

fn soma(x:i32, y:i32) ->  i32 {
  let mut a:i32; // Variável Local
  a = x + y; 
  println!(a); // Imprime 7
  return a;
}

fn main() ->  () {
  let mut a:i32;
  {
    let mut b:i32;
    a = 3;
    b = soma(a, 4);
    println!(b); // Imprime 7
  }
  println!(a); // Imprime 3
  println!(b); // Imprime 5
}