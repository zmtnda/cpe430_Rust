#[allow(dead_code)]
enum ExprC {
    NumC {n: i32},
    BoolC {b: bool},
    IdC {s: char},
    IfC {c: Option<Box<ExprC>>, t: Option<Box<ExprC>>, f: Option<Box<ExprC>>}
}
fn main() {
    // The statements here will be executed when the compiled binary is called

    // Print text to the console
    println!("Hello World!");
    let x = 5 + /* 90 + */ 2;
     println!("Is `x` 10 or 100? x = {}", x);
       println!("{0}, this is {1}. {1}, this is {0}", "Alice", "Bob");
}
