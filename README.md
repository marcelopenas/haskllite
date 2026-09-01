# Haskllite <img src="img/haskllite.svg" align="right" width="150" alt="Haskllite logo">

Haskllite is a lightweight, educational interpreter/compiler for a programming language inspired by Rust's syntax, implemented in Haskell. It serves as a simplified subset designed to demonstrate core compiler concepts including lexical analysis, parsing, and AST interpretation.

## Features

* **Functions:** Declaration support with parameters and explicit return types.
* **Variable Binding:** Logic for `let` (immutable) and `let mut` (mutable) bindings.
* **Strong Typing:** Native support for `i32`, `f64`, `bool`, and `str`.
* **Type Casting:** Explicit and implicit type casting capabilities.
* **Control Flow:** Native `if/else` logic, `while` loops, and C-style `for` loops.
* **Ternary Assignment:** Support for `if/else` expressions during variable assignment.
* **I/O Operations:** Basic `println!()` and `scanln!()` macros.
* **Error Reporting:** Descriptive compilation error messages featuring type info and formatted code snippets showing the error location.

> [!NOTE]
> **Compatibility:** While Haskllite mimics Rust syntax, it is a simplified subset. It does not include a borrow checker, ownership system, or complex lifetimes.

## Running

Ensure you have [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/) installed.

As this is a project with no dependencies its easier to run wih the haskell interpreter, version 9.14.1 is recommended.

### Interpreter Mode

Haskllite can run as an interpreter, this wont produce code and will run entirely on memory

The following command runs the source as an interpreter

    runghc ./Main.hs ./path_to_source.rs

### Compiler Mode

Haskllite can also run as a compiler, it will generate x86 ASM that can be converted to an executable.

> [!IMPORTANT]
> Notice that some features may not work with this mode.

The following command produces the executable:

    bash compile.sh ./path_to_source.rs

### Example code

This is an example of the Rust inspired language that can be executed with Haskllite. It, along with more examples are available under [examples](examples/readme_demo.rs), showcasing all the available features.

```rust
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
```

## Architecture

The project follows a standard compiler pipeline:

1. Pre-processing: Replaces constants and removes comments.

2. Lexer: Tokenizes the raw input string.

3. Parser: Recursive descent parser based on the EBNF below.

4. AST: Generates an Abstract Syntax Tree representing the program.

5. Interpreter: Traverses the AST executing the logic or generating the ASM.

## Grammar specifications

### EBNF

The formal grammar for Haskllite is defined as:

```mermaid
railroad-ebnf-beta

(* High-Level Structure *)
PROGRAM     = { FUNC | STMT } ;

FUNC        = "fn", IDENTIFIER, "(", [ ARGS ], ")", [ "->", ( TYPE | "()" ) ], BLOCK ;

ARGS        = IDENTIFIER, ":", TYPE, { ",", IDENTIFIER, ":", TYPE } ;

BLOCK       = "{", { STMT }, "}" ;

(* Statements *)
STMT        = [ "let", [ "mut" ], IDENTIFIER, ":", TYPE, [ "=", BEXPR ]
              | IDENTIFIER, ( "=" , BEXPR | "(", [ BEXPR, { ",", BEXPR } ], ")" )
              | "println!", "(", BEXPR, ")" 
              | "while", "(", BEXPR, ")", STMT 
              | "for", "(", ASSIGNMENT, ";", BEXPR, ";", ASSIGNMENT, ")", BLOCK
              | "if", "(", BEXPR, ")", STMT, [ "else", STMT ] 
              | "return", BEXPR 
              | BLOCK 
              ], ";" ;

ASSIGNMENT  = IDENTIFIER, "=", BEXPR ;

(* Expression Hierarchy (Precedence) *)
BEXPR       = BTERM, { "||", BTERM } ;
BTERM       = REXPR, { "&&", REXPR } ;
REXPR       = EXPR,  { ("==" | ">" | "<"), EXPR } ;
EXPR        = TERM,  { ("+" | "-" | "^"), TERM } ;
TERM        = POWER, { ("*" | "/"), POWER } ;
POWER       = FACTOR, { "**", FACTOR } ;

FACTOR      = INTEGER 
            | BOOL 
            | STR 
            | CALL 
            | IDENTIFIER 
            | ( "+" | "-" | "!" ), FACTOR 
            | "(", BEXPR, ")" 
            | "scanln!", "(", ")" ;

CALL        = IDENTIFIER, "(", [ BEXPR, { ",", BEXPR } ], ")" ;

(* Lexical Tokens *)
TYPE        = "str" | "i32" | "f64" | "bool" ;
BOOL        = "true" | "false" ;
STR         = '"', { '\"' | ANY - '"' }, '"' ;
IDENTIFIER  = LETTER, { LETTER | DIGIT | "_" } ;
INTEGER     = DIGIT, { DIGIT } ;
LETTER      = "a" | "b" | "..." | "z" | "A" | "B" | "..." | "Z" ;
DIGIT       = "0" | "1" | "..." | "9" ;
```
