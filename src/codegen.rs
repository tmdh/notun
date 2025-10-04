use crate::type_checker::{
    Type, TypedDeclaration, TypedExpression, TypedFunction, TypedModule, TypedStatement,
};
use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{BasicValue, IntValue},
};
use std::process::Command;

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn compile_module(&self, typed_module: &TypedModule) {
        for declaration in &typed_module.declarations {
            match declaration {
                TypedDeclaration::Function(function) => {
                    self.compile_function(&function);
                }
            }
        }

        if let Err(e) = self.module.verify() {
            eprintln!("Module verification failed: {}", e.to_string());
            return;
        }

        println!("Generated LLVM IR:");
        println!("{}", self.module.print_to_string().to_string());

        let target_triple = inkwell::targets::TargetMachine::get_default_triple();
        inkwell::targets::Target::initialize_native(
            &inkwell::targets::InitializationConfig::default(),
        )
        .expect("Failed to initialize native target");

        let target = inkwell::targets::Target::from_triple(&target_triple)
            .expect("Failed to create target from triple");

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Failed to create target machine");

        if !std::fs::exists("notun-cache").unwrap() {
            std::fs::create_dir("notun-cache");
        }

        target_machine
            .write_to_file(
                &self.module,
                inkwell::targets::FileType::Object,
                "notun-cache/program.o".as_ref(),
            )
            .expect("Failed to write object file");

        println!("Object file written to program.o");

        let link_result = Command::new("clang")
            .args(&[
                "notun-cache/program.o",
                "-o",
                "notun-cache/program",
                "-fuse-ld=lld",
            ])
            .output();

        match link_result {
            Ok(output) => {
                if output.status.success() {
                    println!("Successfully linked executable: ./program");
                } else {
                    eprintln!("Linking failed:");
                    eprintln!("{}", String::from_utf8_lossy(&output.stderr));
                }
            }
            Err(e) => {
                eprintln!("Failed to run clang: {}", e);
                eprintln!("Make sure clang and lld are installed");
            }
        }
    }

    pub fn compile_function(&self, function: &TypedFunction) -> inkwell::values::FunctionValue<'_> {
        let type_ = match *function.return_type {
            Type::Int64 => self.context.i64_type(),
            // Type::Float64 => self.context.f64_type(),
            // Type::Bool => self.context.bool_type(),
            _ => unimplemented!(),
        };

        let fn_type = type_.fn_type(&[], false);
        let fn_val = self.module.add_function(&function.name, fn_type, None);
        let basic_block = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(basic_block);

        for statement in &function.body {
            self.compile_statement(statement);
        }

        fn_val
    }

    fn compile_statement(&self, statement: &TypedStatement) {
        match statement {
            TypedStatement::Return { return_value } => {
                self.builder
                    .build_return(Some(&self.compile_expression(return_value)));
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&self, expression: &TypedExpression) -> impl BasicValue<'_> {
        match expression {
            TypedExpression::Integer { value, type_ } => {
                let negative = if *value < 0 { true } else { false };
                self.context.i64_type().const_int(*value as u64, negative)
            }
            _ => unimplemented!(),
        }
    }
}
