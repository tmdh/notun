use crate::{
    ast::BinOp,
    type_checker::{
        Type, TypedDeclaration, TypedExpression, TypedFunction, TypedModule, TypedStatement,
    },
};
use inkwell::{
    FloatPredicate, IntPredicate, OptimizationLevel, builder::Builder, context::Context, execution_engine::ExecutionEngine, module::Module, types::{BasicType, BasicTypeEnum}, values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, process::Command};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub environment: HashMap<String, PointerValue<'ctx>>
}

impl<'ctx> Codegen<'ctx> {
    pub fn compile_module(&mut self, typed_module: &TypedModule) {
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

    pub fn llvm_basic_type(&self, type_: &Type) -> BasicTypeEnum<'ctx> {
        match type_ {
            Type::Int64 => self.context.i64_type().into(),
            Type::Float64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            _ => unimplemented!(),
        }
    }

    pub fn compile_function(&mut self, function: &TypedFunction) -> FunctionValue<'ctx> {
        self.environment.clear();
        let fn_type = self.llvm_basic_type(&function.return_type).fn_type(&[], false);
        let fn_val = self.module.add_function(&function.name, fn_type, None);
        let basic_block = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(basic_block);

        for statement in &function.body {
            self.compile_statement(statement);
        }

        fn_val
    }

    fn compile_statement(&mut self, statement: &TypedStatement) {
        match statement {
            TypedStatement::Return { return_value } => {
                let return_value = &self.compile_expression(return_value);
                self.builder
                    .build_return(Some(return_value));
            }
            TypedStatement::Let { name, type_, value } => {
                let let_value = self.compile_expression(value);
                let let_type = self.llvm_basic_type(type_);
                let let_ptr = self.builder.build_alloca(let_type, name).unwrap();
                self.builder.build_store(let_ptr, let_value).unwrap();
                self.environment.insert(name.clone(), let_ptr);
            }
            TypedStatement::Assignment { lhs, type_, rhs } => {
                let rhs_value = self.compile_expression(rhs);
                let ptr = *self.environment.get(lhs).unwrap();
                self.builder.build_store(ptr, rhs_value);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&mut self, expression: &TypedExpression) -> BasicValueEnum<'ctx> {
        match expression {
            TypedExpression::Integer { value, type_ } => {
                let negative = if *value < 0 { true } else { false };
                self.context
                    .i64_type()
                    .const_int(*value as u64, negative)
                    .into()
            }
            TypedExpression::Float { value, type_ } => {
                self.context.f64_type().const_float(*value).into()
            }
            TypedExpression::Bool { value, type_ } => self
                .context
                .bool_type()
                .const_int(*value as u64, false)
                .into(),
            TypedExpression::BinaryOperation {
                operator,
                left,
                right,
                type_,
            } => {
                let left = self.compile_expression(left);
                let right = self.compile_expression(right);
                match (left, right) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        self.compile_int_binary_operation(*operator, l, r) // handles integer and boolean values
                    }
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        self.compile_float_binary_operation(*operator, l, r)
                    }
                    _ => unreachable!(),
                }
            }
            TypedExpression::Var { name, type_ } => {
                let let_type = self.llvm_basic_type(type_);
                let ptr = *self.environment.get(name).unwrap();
                self.builder.build_load(let_type, ptr, name).unwrap()
            }
            _ => unimplemented!()
        }
    }

    fn compile_int_binary_operation(
        &self,
        operator: BinOp,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match operator {
            BinOp::Add => self.builder.build_int_add(left, right, "add"),
            BinOp::Subtract => self.builder.build_int_sub(left, right, "sub"),
            BinOp::Multiply => self.builder.build_int_mul(left, right, "mul"),
            BinOp::Divide => self.builder.build_int_signed_div(left, right, "div"),
            BinOp::Modulo => self.builder.build_int_signed_rem(left, right, "rem"),
            BinOp::LogicalOr => self.builder.build_or(left, right, "logical"),
            BinOp::LogicalAnd => self.builder.build_and(left, right, "logical"),
            BinOp::Less => self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "cmp"),
            BinOp::Greater => self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "cmp"),
            BinOp::LessEqual => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp")
            }
            BinOp::GreaterEqual => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp")
            }
            BinOp::EqualEqual => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp")
            }
            BinOp::NotEqual => self
                .builder
                .build_int_compare(IntPredicate::NE, left, right, "cmp"),
            _ => unreachable!(),
        }
        .unwrap()
        .into()
    }

    fn compile_float_binary_operation(
        &self,
        operator: BinOp,
        left: FloatValue<'ctx>,
        right: FloatValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match operator {
            BinOp::Add => self
                .builder
                .build_float_add(left, right, "add")
                .unwrap()
                .into(),
            BinOp::Subtract => self
                .builder
                .build_float_sub(left, right, "sub")
                .unwrap()
                .into(),
            BinOp::Multiply => self
                .builder
                .build_float_mul(left, right, "mul")
                .unwrap()
                .into(),
            BinOp::Divide => self
                .builder
                .build_float_div(left, right, "div")
                .unwrap()
                .into(),
            BinOp::Modulo => self
                .builder
                .build_float_rem(left, right, "rem")
                .unwrap()
                .into(),
            BinOp::Less => self
                .builder
                .build_float_compare(FloatPredicate::OLT, left, right, "cmp")
                .unwrap()
                .into(),
            BinOp::Greater => self
                .builder
                .build_float_compare(FloatPredicate::OGT, left, right, "cmp")
                .unwrap()
                .into(),
            BinOp::LessEqual => self
                .builder
                .build_float_compare(FloatPredicate::OLE, left, right, "cmp")
                .unwrap()
                .into(),
            BinOp::GreaterEqual => self
                .builder
                .build_float_compare(FloatPredicate::OGE, left, right, "cmp")
                .unwrap()
                .into(),
            BinOp::EqualEqual => self
                .builder
                .build_float_compare(FloatPredicate::OEQ, left, right, "cmp")
                .unwrap()
                .into(),
            BinOp::NotEqual => self
                .builder
                .build_float_compare(FloatPredicate::ONE, left, right, "cmp")
                .unwrap()
                .into(),
            _ => unreachable!(),
        }
    }
}
