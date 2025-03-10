#![allow(unused)]
use type_checker::TypeChecker;
use ir::Translator;

mod ast;
mod environment;
mod ir;
mod lexer;
mod parser;
mod type_checker;

fn main() {
    // const SOURCE: &str = "
    // let a: Int64 = 50

    // fn main(a: Int64, b: (Bool, Int64)) -> (Int64, Int64) {
    //     let a: Int64 = 1 + 2 * 3 + 5
    //     let b: Int64 = 50 * -99
    //     let b: Int64 = -a

    //     if a == b {
    //         let b: Int64 = 6
    //     }
    //     while a == b {
    //         let c: Int64 = !true
    //     }
    //     let a: Int64 = sqrt(9) + 4
    //     let b: Int64 = (9, true)
    //     sqrt(9)
    //     let a: Int64[3][3] = [[1, 2, 3]]
    //     a = sqrt([9, 16])[0]
    //     a = a[0].0.1
    // }";
    const SOURCE: &str = "
    fn main(a: Int64, b: (Bool, Int64)) -> (Int64, Int64) {
        let a: Int64 = 1 + 2 * 3 + 5
        let b: Int64 = 50 * -99
        let b: Int64 = -a
        let f: Bool = true
        if (f) {
            b = 9
        } else {
            b = 6
        }
        while (f) {
            b = b + 1
            f = false
        }
        return (5, 5)
    }";
    let tokenizer = lexer::make_tokenizer(SOURCE);
    let mut parser = parser::Parser::new(tokenizer);
    let module = parser.parse_module().expect("Error parsing");
    let mut type_checker = TypeChecker::new();
    let typed_module = type_checker.visit_module(module);
    if type_checker.errors.len() > 0 {
        for error in type_checker.errors {
            println!("{:#?}", error);
        }
    } else {
        let typed_module = typed_module.unwrap();
        println!("Typed AST:");
        println!("{:#?}", typed_module);
        
        // Translate to IR
        let mut translator = Translator::new();
        let ir = translator.translate_module(&typed_module);
        
        println!("\nIntermediate Representation:");
        for stmt in ir {
            println!("{:#?}", stmt);
        }
    }
}
