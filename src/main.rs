mod ast;
mod lexer;
mod parser;

fn main() {
    /*    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99
    let c : bool = 5 == 5
    let d : bool = 6 <= 6 && 9 >= 10 */
    const SOURCE: &str = "fn main(a: i64, b: (bool, u64)) -> (u64, u64) {

    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99

    if a == b {
        let b : u64 = 6
    }
    while a == b {
        let c : u64 = !true
    }
    let a : u64 = sqrt(9) + 4
    let b : u64 = a.0.1
    sqrt(9)
}";
    let tokenizer = lexer::make_tokenizer(SOURCE);
    let mut parser = parser::Parser::new(tokenizer);
    let module = parser.parse_module();
    println!("{:#?}", module);
}
