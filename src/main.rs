#![allow(unused)]

mod ast;
mod codegen;
mod environment;
mod lexer;
mod parser;
mod type_checker;

use inkwell::context::Context;
use std::{env, process::Command};
use type_checker::TypeChecker;

use crate::codegen::Codegen;

fn main() {
    let command = env::args().nth(1).unwrap();
    let filename = env::args().nth(2).unwrap();
    let source = std::fs::read_to_string(filename).unwrap();
    let tokenizer = lexer::make_tokenizer(source.as_str());
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

        let context = Context::create();
        let codegen = Codegen {
            context: &context,
            module: context.create_module("main_module"),
            builder: context.create_builder(),
        };

        codegen.compile_module(&typed_module);

        if command == "run" {
            println!("Running program...");
            let status = Command::new("notun-cache/program").status().unwrap();
            println!("Exit status: {}", status.code().unwrap());
        }
    }
}
