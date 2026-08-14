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

use crate::{codegen::Codegen, lexer::TokenKind::Comma};

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
        let mut typed_module = typed_module.unwrap();
        println!("{:#?}", typed_module);

        typed_module.add_entry_point();

        let context = Context::create();
        let mut codegen = Codegen::new(&context);

        codegen.compile_module(&typed_module);

        let compile_runtime_result = Command::new("clang")
            .args(&["-c", "runtime.c", "-o", "notun-cache/runtime.o"])
            .output();

        match compile_runtime_result {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Compiling runtime failed:");
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                }
            }
            Err(e) => {
                eprintln!("Failed to run clang: {}", e);
                eprintln!("Make sure clang and lld are installed");
            }
        }

        let link_result = Command::new("clang")
            .args(&[
                "notun-cache/program.o",
                "notun-cache/runtime.o",
                "-o",
                "notun-cache/program",
                "-fuse-ld=lld",
            ])
            .output();

        match link_result {
            Ok(output) => {
                if !output.status.success() {
                    eprintln!("Linking failed:");
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                }
            }
            Err(e) => {
                eprintln!("Failed to run clang: {}", e);
                eprintln!("Make sure clang and lld are installed");
            }
        }

        if command == "run" {
            println!("Running program...");
            let status = Command::new("notun-cache/program").status().unwrap();
            println!("Exit status: {}", status.code().unwrap());
        }
    }
}
