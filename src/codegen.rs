use crate::{
    ast::BinOp,
    type_checker::{
        Type, TypedDeclaration, TypedExpression, TypedFunction, TypedModule, TypedStatement,
    },
};
use inkwell::{
    FloatPredicate, IntPredicate, OptimizationLevel,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, fs, process::Command};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub env: HashMap<String, PointerValue<'ctx>>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Codegen<'ctx> {
        Codegen {
            context,
            module: context.create_module("main_module"),
            builder: context.create_builder(),
            env: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn compile_module(&mut self, typed_module: &TypedModule) {
        let print_i64_fn = self.module.add_function(
            "print",
            self.context
                .void_type()
                .fn_type(&[self.context.i64_type().into()], false),
            None,
        );
        self.functions.insert("print".to_string(), print_i64_fn);

        for declaraion in &typed_module.declarations {
            match declaraion {
                TypedDeclaration::Function(function) => {
                    let param_types: Vec<_> = function
                        .parameters
                        .iter()
                        .map(|parameter| self.llvm_basic_type(&parameter.type_).into())
                        .collect();
                    let fn_type = match function.return_type.as_ref() {
                        Type::Unit => self.context.void_type().fn_type(&param_types, false),
                        _ => self
                            .llvm_basic_type(&function.return_type)
                            .fn_type(&param_types, false),
                    };
                    let fn_value = self.module.add_function(&function.name, fn_type, None);
                    self.functions.insert(function.name.clone(), fn_value);
                }
            }
        }

        for declaration in &typed_module.declarations {
            match declaration {
                TypedDeclaration::Function(function) => {
                    self.compile_function(&function);
                }
            }
        }

        println!("{}", self.module.print_to_string().to_string());

        if let Err(e) = self.module.verify() {
            eprintln!("Module verification failed: {}", e.to_string());
            return;
        }

        let target_triple = TargetMachine::get_default_triple();
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");

        let target =
            Target::from_triple(&target_triple).expect("Failed to create target from triple");

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("Failed to create target machine");

        if !fs::exists("notun-cache").unwrap() {
            fs::create_dir("notun-cache");
        }

        target_machine
            .write_to_file(
                &self.module,
                FileType::Object,
                "notun-cache/program.o".as_ref(),
            )
            .expect("Failed to write object file");
    }

    pub fn llvm_basic_type(&self, type_: &Type) -> BasicTypeEnum<'ctx> {
        match type_ {
            Type::Int64 => self.context.i64_type().into(),
            Type::Float64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Tuple { types } => {
                let field_types: Vec<BasicTypeEnum<'_>> =
                    types.iter().map(|t| self.llvm_basic_type(t)).collect();
                self.context
                    .struct_type(&field_types.as_slice(), false)
                    .into()
            }
            Type::Array { element_type, size } => 
                self.llvm_basic_type(element_type)
                .array_type(*size as u32)
                .into(),
            
            e => unimplemented!("{e:?}"),
        }
    }

    pub fn compile_function(&mut self, function: &TypedFunction) -> FunctionValue<'ctx> {
        let fn_value = *self.functions.get(&function.name).unwrap();
        let basic_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(basic_block);
        let old_env = self.env.clone();
        for (index, parameter) in function.parameters.iter().enumerate() {
            let param_value = fn_value.get_nth_param(index as u32).unwrap();
            let param_type = self.llvm_basic_type(&parameter.type_);
            let param_ptr = self
                .builder
                .build_alloca(param_type, &parameter.name)
                .unwrap();
            self.builder.build_store(param_ptr, param_value);
            self.env.insert(parameter.name.clone(), param_ptr);
        }
        self.compile_statements(&function.body, fn_value);
        self.env = old_env;
        if !self.is_current_block_terminated() {
            match function.return_type.as_ref() {
                Type::Unit => {
                    self.builder.build_return(None);
                }
                _ => {
                    self.builder.build_unreachable();
                }
            }
        }
        fn_value
    }

    pub fn compile_statements(
        &mut self,
        statements: &Vec<TypedStatement>,
        fn_value: FunctionValue<'ctx>,
    ) {
        for statement in statements {
            self.compile_statement(statement, fn_value);
        }
    }

    fn is_current_block_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .and_then(|block| block.get_terminator())
            .is_some()
    }

    fn compile_statement(&mut self, statement: &TypedStatement, fn_value: FunctionValue<'ctx>) {
        match statement {
            TypedStatement::Return { return_value } => {
                let return_value = &self.compile_expression(return_value).unwrap();
                self.builder.build_return(Some(return_value));
            }
            TypedStatement::Let { name, type_, value } => {
                let let_value = self.compile_expression(value).unwrap();
                let let_type = self.llvm_basic_type(type_);
                let let_ptr = self.builder.build_alloca(let_type, name).unwrap();
                self.builder.build_store(let_ptr, let_value).unwrap();
                self.env.insert(name.clone(), let_ptr);
            }
            TypedStatement::Assignment { lhs, type_, rhs } => {
                let rhs_value = self.compile_expression(rhs).unwrap();
                let mut ptr = *self.env.get(&lhs.name).unwrap();
                let mut current_type = lhs.root_type.clone();
                for index in &lhs.path {
                    let struct_type = self.llvm_basic_type(&current_type);
                    ptr = self
                        .builder
                        .build_struct_gep(struct_type, ptr, *index, "field")
                        .unwrap();
                    current_type = current_type.tuple_types().unwrap()[*index as usize].clone();
                }
                self.builder.build_store(ptr, rhs_value);
            }
            TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.compile_expression(condition).unwrap().into_int_value();
                let then_block = self.context.append_basic_block(fn_value, "then");
                let else_block = self.context.append_basic_block(fn_value, "else");
                let merge_block = self.context.append_basic_block(fn_value, "merge");
                self.builder
                    .build_conditional_branch(condition, then_block, else_block);

                self.builder.position_at_end(then_block);
                self.compile_statements(then_branch, fn_value);
                if (!self.is_current_block_terminated()) {
                    self.builder.build_unconditional_branch(merge_block);
                }

                self.builder.position_at_end(else_block);
                if let Some(else_branch) = else_branch {
                    self.compile_statements(else_branch, fn_value);
                }
                if (!self.is_current_block_terminated()) {
                    self.builder.build_unconditional_branch(merge_block);
                }
                self.builder.position_at_end(merge_block);
            }
            TypedStatement::While { condition, body } => {
                let condition_block = self.context.append_basic_block(fn_value, "condition");
                let body_block = self.context.append_basic_block(fn_value, "body");
                let end_block = self.context.append_basic_block(fn_value, "end");
                self.builder.build_unconditional_branch(condition_block);

                self.builder.position_at_end(condition_block);
                let condition = self.compile_expression(condition).unwrap().into_int_value();
                self.builder
                    .build_conditional_branch(condition, body_block, end_block);

                self.builder.position_at_end(body_block);
                self.compile_statements(body, fn_value);
                if (!self.is_current_block_terminated()) {
                    self.builder.build_unconditional_branch(condition_block);
                }

                self.builder.position_at_end(end_block);
            }
            TypedStatement::Expression(e) => {
                self.compile_expression(e);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&mut self, expression: &TypedExpression) -> Option<BasicValueEnum<'ctx>> {
        match expression {
            TypedExpression::Integer { value, type_ } => {
                let negative = if *value < 0 { true } else { false };
                Some(
                    self.context
                        .i64_type()
                        .const_int(*value as u64, negative)
                        .into(),
                )
            }
            TypedExpression::Float { value, type_ } => {
                Some(self.context.f64_type().const_float(*value).into())
            }
            TypedExpression::Bool { value, type_ } => Some(
                self.context
                    .bool_type()
                    .const_int(*value as u64, false)
                    .into(),
            ),
            TypedExpression::BinaryOperation {
                operator,
                left,
                right,
                type_,
            } => {
                let left = self.compile_expression(left).unwrap();
                let right = self.compile_expression(right).unwrap();
                match (left, right) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        Some(self.compile_int_binary_operation(*operator, l, r)) // handles integer and boolean values
                    }
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        Some(self.compile_float_binary_operation(*operator, l, r))
                    }
                    _ => unreachable!(),
                }
            }
            TypedExpression::Var { name, type_ } => {
                let let_type = self.llvm_basic_type(type_);
                let ptr = *self.env.get(name).unwrap();
                Some(self.builder.build_load(let_type, ptr, name).unwrap())
            }
            TypedExpression::Call {
                name,
                arguments,
                type_,
            } => {
                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|argument| self.compile_expression(argument).unwrap().into())
                    .collect();
                self.builder
                    .build_direct_call(
                        *self.functions.get(name).unwrap(),
                        arguments.as_slice(),
                        "call",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .basic()
            }
            TypedExpression::Tuple { values, type_ } => {
                let tuple_basic_type = self.llvm_basic_type(type_);
                let ptr = self
                    .builder
                    .build_alloca(tuple_basic_type, "tuple")
                    .unwrap();
                for (index, value) in values.iter().enumerate() {
                    let expression = self.compile_expression(value).unwrap();
                    let value_ptr = self
                        .builder
                        .build_struct_gep(
                            tuple_basic_type,
                            ptr,
                            index as u32,
                            format!("tuple.{index}").as_str(),
                        )
                        .unwrap();
                    self.builder.build_store(value_ptr, expression);
                }
                Some(
                    self.builder
                        .build_load(tuple_basic_type, ptr, "tuple")
                        .unwrap(),
                )
            }
            TypedExpression::TupleIndex {
                tuple,
                index,
                type_,
            } => {
                let lhs = self.compile_expression(tuple).unwrap();
                Some(
                    self.builder
                        .build_extract_value(
                            lhs.into_struct_value(),
                            *index,
                            format!("tuple.{index}").as_str(),
                        )
                        .unwrap(),
                )
            }
            TypedExpression::NegateMinus { value, type_ } => {
                let value = self.compile_expression(value).unwrap();
                match type_.as_ref() {
                    &Type::Int64 => Some(
                        self.builder
                            .build_int_neg(value.into_int_value(), "negative")
                            .unwrap()
                            .into(),
                    ),
                    &Type::Float64 => Some(
                        self.builder
                            .build_float_neg(value.into_float_value(), "negative")
                            .unwrap()
                            .into(),
                    ),
                    _ => unreachable!(),
                }
            }
            TypedExpression::NegateBang { value, type_ } => {
                let value = self.compile_expression(value).unwrap();
                match type_.as_ref() {
                    &Type::Bool => Some(
                        self.builder
                            .build_not(value.into_int_value(), "not")
                            .unwrap()
                            .into(),
                    ),
                    _ => unreachable!(),
                }
            }
            TypedExpression::Array { values, type_ } => {
                let array_basic_type = self.llvm_basic_type(type_);
                let ptr = self.builder.build_alloca(array_basic_type, "array").unwrap();
                let zero = self.context.i64_type().const_int(0, false);

                for (index, value) in values.iter().enumerate() {
                    let expression = self.compile_expression(value).unwrap();
                    let idx = self.context.i64_type().const_int(index as u64, false);
                    let value_ptr = unsafe {
                        self.builder.build_gep(array_basic_type, ptr, &[zero, idx], "array_init_elem").unwrap()
                    };
                    self.builder.build_store(value_ptr, expression);
                }
                Some(self.builder.build_load(array_basic_type, ptr, "array_val").unwrap())
            }
            TypedExpression::ArraySubscript { array, index, type_ } => {
                let array_value = self.compile_expression(array).unwrap();
                let index_value = self.compile_expression(index).unwrap();

                let element_basic_type = self.llvm_basic_type(type_);
                let array_basic_type = self.llvm_basic_type(&array.type_());

                let temp_ptr = self.builder.build_alloca(array_basic_type, "temp_array").unwrap();
                self.builder.build_store(temp_ptr, array_value).unwrap();

                let zero = self.context.i64_type().const_int(0, false);

                let ptr = unsafe {
                    self.builder.build_gep(array_basic_type, temp_ptr, &[zero, index_value.into_int_value()], "array_elem").unwrap()
                };
                Some(self.builder.build_load(element_basic_type, ptr, "array_elem_val").unwrap())
            }
            c => unimplemented!("Codegen for {:?} is unimplemented", c),
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
