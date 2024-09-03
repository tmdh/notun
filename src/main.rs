mod ast;
mod lexer;
mod parser;

fn main() {
    const SOURCE: &str = "fn main(a: i64, b: (bool, u64)) -> (u64, u64) {
    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99
    let c : bool = 5 == 5
    let d : bool = 6 <= 6 && 9 >= 10

    if a == b {
        let b : u64 = 6
    }

    while a == b {
        let c : u64 = !true
    }

    return 0
    let a : u64 = sqrt(9) + b
}";
    let tokenizer = lexer::make_tokenizer(SOURCE);
    let mut parser = parser::Parser::new(tokenizer);
    let module = parser.parse_module();
    println!("{:#?}", module);
}
