mod ast;
mod lexer;
mod parser;

fn main() {
    const SOURCE: &str = "let a : u64 = 50
fn main(a: i64, b: (bool, u64)) -> (u64, u64) {

    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99
    let b : i64 = -a

    if a == b {
        let b : u64 = 6
    }
    while a == b {
        let c : u64 = !true
    }
    let a : u64 = sqrt(9) + 4
    let b : u64 = (9, true)
    sqrt(9)

    let a: u64[3][3] = [[1, 2, 3]]
    a = sqrt([9, 16])[0]
    a = a[0].0.1
}";
    let tokenizer = lexer::make_tokenizer(SOURCE);
    let mut parser = parser::Parser::new(tokenizer);
    let module = parser.parse_module();
    println!("{:#?}", module);
}
