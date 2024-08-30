mod ast;
mod lexer;
mod parser;

fn main() {
    const SOURCE: &str = "fn main() {
    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99
    let c : bool = 5 == 5
    let d : bool = 6 <= 6 && 9 >= 10

    if a == b {
        let b : u64 = 6
    }

    while a == b {
        let c : u64 = 9
    }
}";
    let tokenizer = lexer::make_tokenizer(SOURCE);
    let mut parser = parser::Parser::new(tokenizer);
    let module = parser.parse_module();
    println!("{:#?}", module);
}
