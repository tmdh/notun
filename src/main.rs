use chumsky::Parser;
use std::{env, fs};

mod parser;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, mut errs) = parser::lexer().parse(src.as_str()).into_output_errors();

    println!("{:?}", tokens);
}
