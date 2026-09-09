use crate::{
    ast::{
        ArrayTypeAst, BinOp, Constant, Declaration, Expression, Function, Module, Statement,
        TupleTypeAst, TypeAst,
    },
    environment::Environment,
};
use std::{collections::HashSet, rc::Rc};

pub struct TypeChecker {
    pub errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker { errors: vec![] }
    }

    pub fn visit_module(&mut self, module: Module) -> Option<TypedModule> {
        let mut env = Environment::new();
        // register structs here
        let mut functions = vec![];
        let mut constants = vec![];
        for declaration in module.declarations {
            match declaration {
                Declaration::Function(function) => functions.push(function),
                Declaration::Constant(constant) => constants.push(constant),
            }
        }

        let print_parameter_type = Rc::new(Type::Int64);
        env.insert(
            "print".to_string(),
            ValueConstructorVariant::ModuleFunction {
                parameters: vec![TypedParameter {
                    name: "print".to_string(),
                    type_: print_parameter_type.clone(),
                }],
            },
            Rc::new(Type::Fn {
                parameters: vec![print_parameter_type.clone()],
                return_type: Rc::new(Type::Unit),
            }),
        );

        for function in &functions {
            match self.register_function(function.clone(), &mut env) {
                RegistrationStatus::Failure => {
                    return None;
                }
                _ => {}
            }
        }

        for constant in constants {
            match self.register_constant(constant, &mut env) {
                RegistrationStatus::Failure => {
                    return None;
                }
                _ => {}
            }
        }

        let mut typed_declarations = vec![];

        for function in &functions {
            typed_declarations.push(self.infer_function(function, &mut env));
        }

        if self.errors.len() > 0 {
            None
        } else {
            Some(TypedModule {
                declarations: typed_declarations,
            })
        }
    }

    fn register_function(
        &mut self,
        function: Function,
        env: &mut Environment,
    ) -> RegistrationStatus {
        let Function {
            name,
            parameters,
            return_type,
            ..
        } = function;
        let mut parameter_types = vec![];
        for parameter in &parameters {
            if let Some(type_) = self.type_from_ast(&parameter.type_) {
                parameter_types.push(type_);
            } else {
                return RegistrationStatus::Failure;
            }
        }
        if let Some(type_) = self.type_from_ast(&return_type) {
            parameter_types.push(type_);
        } else {
            return RegistrationStatus::Failure;
        }
        if let Some(return_type) = self.type_from_ast(&return_type) {
            let typed_parameters = parameters
                .iter()
                .zip(&parameter_types)
                .map(|(p, t)| TypedParameter {
                    name: p.name.clone(),
                    type_: t.clone(),
                })
                .collect::<Vec<_>>();
            let type_ = Type::Fn {
                parameters: parameter_types,
                return_type,
            };
            env.insert(
                name.clone(),
                ValueConstructorVariant::ModuleFunction {
                    parameters: typed_parameters,
                },
                Rc::new(type_),
            );
            return RegistrationStatus::Success;
        } else {
            RegistrationStatus::Failure
        }
    }

    fn register_constant(
        &mut self,
        constant: Constant,
        env: &mut Environment,
    ) -> RegistrationStatus {
        let Constant { name, type_, .. } = constant;
        if let Some(type_) = self.type_from_ast(&type_) {
            env.insert(name.clone(), ValueConstructorVariant::ModuleConstant, type_);
            return RegistrationStatus::Success;
        }
        RegistrationStatus::Failure
    }

    fn type_from_ast(&mut self, type_ast: &TypeAst) -> Option<Rc<Type>> {
        let type_ = match type_ast {
            TypeAst::Constructor(constructor_type_ast) => {
                match constructor_type_ast.name.as_str() {
                    "Int64" => Type::Int64,
                    "Float64" => Type::Float64,
                    "Bool" => Type::Bool,
                    _ => {
                        self.errors.push(TypeError::UnknownType);
                        return None;
                    }
                }
            }
            TypeAst::Tuple(TupleTypeAst { types: type_asts }) => {
                let mut types = vec![];
                for type_ast in type_asts {
                    types.push(self.type_from_ast(type_ast)?);
                }
                Type::Tuple { types }
            }
            TypeAst::Array(ArrayTypeAst { type_, dimensions }) => {
                let mut array_type = self.type_from_ast(type_)?;
                for dimension in dimensions.iter().rev() {
                    array_type = Rc::new(Type::Array {
                        element_type: array_type,
                        size: *dimension,
                    });
                }
                return Some(array_type);
            }
            TypeAst::Unit => Type::Unit,
        };
        Some(Rc::new(type_))
    }

    fn infer_function(&mut self, function: &Function, env: &mut Environment) -> TypedDeclaration {
        let Function {
            name,
            parameters,
            body,
            ..
        } = function;

        let registered_function = env.get(name).expect("Function is not registered");
        let ValueConstructor { type_, .. } = registered_function;
        let (parameter_types, return_type) = type_
            .fn_types()
            .expect("Registered function does not have function type");
        let typed_parameters = parameters
            .iter()
            .zip(parameter_types)
            .map(|(p, t)| TypedParameter {
                name: p.name.clone(),
                type_: t,
            })
            .collect::<Vec<_>>();
        env.current_fn_name = name.clone();
        env.current_fn_return_type = return_type.clone();
        let body =
            self.infer_function_body_scoped(&typed_parameters, &body, return_type.clone(), env);
        env.current_fn_name = "".to_string();
        env.current_fn_return_type = Rc::new(Type::Unit);
        TypedDeclaration::Function(TypedFunction {
            name: name.clone(),
            parameters: typed_parameters,
            return_type: return_type.clone(),
            body,
        })
    }

    fn infer_function_body_scoped(
        &mut self,
        typed_paramters: &Vec<TypedParameter>,
        body: &Vec<Statement>,
        _return_type: Rc<Type>,
        env: &mut Environment,
    ) -> Vec<TypedStatement> {
        env.enter_new_scope(|env| {
            let mut parameter_names: HashSet<&String> =
                HashSet::with_capacity(typed_paramters.len());
            for parameter in typed_paramters {
                if !parameter_names.insert(&parameter.name) {
                    self.errors.push(TypeError::ArgumentNameAlreadyUsed {
                        name: parameter.name.clone(),
                    });
                }
                env.insert_local_variable(parameter.name.clone(), parameter.type_.clone());
            }
            self.infer_statements_scoped(body, env)
        })
    }

    fn infer_statements_scoped(
        &mut self,
        statements: &Vec<Statement>,
        env: &mut Environment,
    ) -> Vec<TypedStatement> {
        env.enter_new_scope(|env| {
            let mut typed_statements: Vec<TypedStatement> = vec![];
            for statement in statements {
                let typed_statement = self.infer_statement(statement, env);
                if let Some(typed_statement) = typed_statement {
                    typed_statements.push(typed_statement);
                }
            }
            typed_statements
        })
    }

    fn infer_statement(
        &mut self,
        statement: &Statement,
        env: &mut Environment,
    ) -> Option<TypedStatement> {
        match statement {
            Statement::Let { name, type_, value } => {
                let typed_value = self.infer_expression(value, env);
                let type_from_annotation = self.type_from_ast(type_);
                if let Some(type_) = &type_from_annotation {
                    env.insert_local_variable(name.clone(), type_.clone());
                }
                match (typed_value, type_from_annotation) {
                    (Some(typed_value), Some(type_from_annotation)) => {
                        if typed_value.type_() == type_from_annotation {
                            return Some(TypedStatement::Let {
                                name: name.clone(),
                                type_: type_from_annotation,
                                value: typed_value,
                            });
                        } else {
                            self.errors.push(TypeError::TypeMismatchAssign {
                                name: name.clone(),
                                expected: type_from_annotation.clone(),
                                found: typed_value.type_().clone(),
                            });
                            return Some(TypedStatement::Let {
                                name: name.clone(),
                                type_: type_from_annotation,
                                value: typed_value,
                            });
                        }
                    }
                    _ => None,
                }
            }
            Statement::Assignment { lhs, rhs } => {
                let lhs_typed = self.infer_expression(lhs, env).unwrap();
                let rhs_typed = self.infer_expression(rhs, env).unwrap();
                let place = self.place_from_expression(&lhs_typed)?;
                let place_type = lhs_typed.type_();
                if rhs_typed.type_() != place_type {
                    self.errors.push(TypeError::TypeMismatchAssign {
                        name: place.name.clone(),
                        expected: place_type.clone(),
                        found: rhs_typed.type_(),
                    });
                }
                Some(TypedStatement::Assignment {
                    lhs: place,
                    type_: place_type,
                    rhs: rhs_typed,
                })
            }
            Statement::Expression(expression) => {
                let typed_expression = self.infer_expression(expression, env)?;
                return Some(TypedStatement::Expression(typed_expression));
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let typed_condition = self.infer_expression(condition, env)?;
                match typed_condition.type_().as_ref() {
                    Type::Bool => {}
                    _ => {
                        self.errors.push(TypeError::TypeMismatchIfCondition {
                            found: typed_condition.type_().clone(),
                        });
                    }
                };
                let then_branch = self.infer_statements_scoped(then_branch, env);
                let else_branch = else_branch
                    .as_ref()
                    .map(|s| self.infer_statements_scoped(s, env));
                Some(TypedStatement::If {
                    condition: typed_condition,
                    then_branch,
                    else_branch,
                })
            }
            Statement::While { condition, body } => {
                let typed_condition = self.infer_expression(condition, env)?;
                match typed_condition.type_().as_ref() {
                    Type::Bool => {}
                    _ => {
                        self.errors.push(TypeError::TypeMismatchWhileCondition {
                            found: typed_condition.type_().clone(),
                        });
                    }
                };
                let body = self.infer_statements_scoped(body, env);
                Some(TypedStatement::While {
                    condition: typed_condition,
                    body,
                })
            }
            Statement::Return { return_value } => {
                let typed_return_value = self.infer_expression(return_value, env)?;
                if typed_return_value.type_() != env.current_fn_return_type {
                    self.errors.push(TypeError::TypeMismatchReturnValue {
                        name: env.current_fn_name.clone(),
                        found: typed_return_value.type_(),
                        expected: env.current_fn_return_type.clone(),
                    });
                }
                Some(TypedStatement::Return {
                    return_value: typed_return_value,
                })
            }
        }
    }

    fn place_from_expression(&mut self, expression: &TypedExpression) -> Option<TypedPlace> {
        match expression {
            TypedExpression::Var { name, type_ } => Some(TypedPlace {
                name: name.clone(),
                root_type: type_.clone(),
                path: vec![],
            }),
            TypedExpression::TupleIndex { tuple, index, .. } => {
                let mut place = self.place_from_expression(tuple)?;
                place.path.push(*index);
                Some(place)
            }
            _ => {
                self.errors.push(TypeError::InvalidAssignmentTarget);
                None
            }
        }
    }

    fn infer_expression(
        &mut self,
        expression: &Expression,
        env: &mut Environment,
    ) -> Option<TypedExpression> {
        match expression {
            Expression::Integer { value } => Some(TypedExpression::Integer {
                value: *value,
                type_: Rc::new(Type::Int64),
            }),
            Expression::Float { value } => Some(TypedExpression::Float {
                value: *value,
                type_: Rc::new(Type::Float64),
            }),
            Expression::Bool { value } => Some(TypedExpression::Bool {
                value: *value,
                type_: Rc::new(Type::Bool),
            }),
            Expression::Var { name } => {
                if let Some(constructor) = env.get(name) {
                    match constructor.variant {
                        ValueConstructorVariant::LocalVariable
                        | ValueConstructorVariant::ModuleConstant => Some(TypedExpression::Var {
                            name: name.clone(),
                            type_: constructor.type_.clone(),
                        }),
                        _ => {
                            self.errors
                                .push(TypeError::ClosureNotSupported { name: name.clone() });
                            None
                        }
                    }
                } else {
                    self.errors
                        .push(TypeError::VariableNotDefined { name: name.clone() });
                    None
                }
            }
            Expression::BinaryOperation {
                operator,
                left,
                right,
            } => {
                let left = self.infer_expression(left, env);
                let right = self.infer_expression(right, env);
                match (left, right) {
                    (Some(left), Some(right)) => {
                        if left.type_() == right.type_() {
                            Some(TypedExpression::BinaryOperation {
                                operator: *operator,
                                type_: match *operator {
                                    BinOp::EqualEqual
                                    | BinOp::Greater
                                    | BinOp::GreaterEqual
                                    | BinOp::Less
                                    | BinOp::LessEqual
                                    | BinOp::LogicalAnd
                                    | BinOp::LogicalOr
                                    | BinOp::NotEqual => Rc::new(Type::Bool),
                                    _ => left.type_(),
                                },
                                left: Box::new(left),
                                right: Box::new(right),
                            })
                        } else {
                            self.errors.push(TypeError::BinaryOpTypeMismatch {
                                left: left.type_(),
                                right: right.type_(),
                            });
                            None
                        }
                    }
                    _ => None,
                }
            }
            Expression::NegateBang { value } => {
                if let Some(value) = self.infer_expression(value, env) {
                    Some(TypedExpression::NegateBang {
                        type_: value.type_().clone(),
                        value: Box::new(value),
                    })
                } else {
                    None
                }
            }
            Expression::NegateMinus { value } => {
                if let Some(value) = self.infer_expression(value, env) {
                    Some(TypedExpression::NegateMinus {
                        type_: value.type_().clone(),
                        value: Box::new(value),
                    })
                } else {
                    None
                }
            }
            Expression::Positive { value } => {
                if let Some(value) = self.infer_expression(value, env) {
                    Some(TypedExpression::Positive {
                        type_: value.type_().clone(),
                        value: Box::new(value),
                    })
                } else {
                    None
                }
            }
            Expression::Call { name, arguments } => {
                let constructor = env.get(name);
                if let Some(constructor) = constructor {
                    match &constructor.variant {
                        ValueConstructorVariant::ModuleFunction { parameters } => {
                            let parameters = parameters.clone();
                            match constructor.type_.as_ref() {
                                Type::Fn { return_type, .. } => {
                                    let return_type = return_type.clone();
                                    let mut typed_arguments = vec![];
                                    if parameters.len() != arguments.len() {
                                        self.errors.push(TypeError::UnexpectedNumberOfArguments {
                                            name: name.clone(),
                                        });
                                    }
                                    let mut problem_count = 0;
                                    for (parameter, argument) in parameters.iter().zip(arguments) {
                                        if let Some(typed_argument) =
                                            self.infer_expression(argument, env)
                                        {
                                            if typed_argument.type_() == parameter.type_ {
                                                typed_arguments.push(typed_argument);
                                            } else {
                                                self.errors.push(
                                                    TypeError::UnexpectedTypeToFunction {
                                                        name: name.clone(),
                                                        parameter_name: parameter.name.clone(),
                                                        expected: parameter.type_.clone(),
                                                        found: typed_argument.type_(),
                                                    },
                                                );
                                                problem_count += 1;
                                            }
                                        }
                                    }
                                    if problem_count > 0 {
                                        return None;
                                    }
                                    Some(TypedExpression::Call {
                                        name: name.clone(),
                                        arguments: typed_arguments,
                                        type_: return_type.clone(),
                                    })
                                }
                                _ => {
                                    self.errors.push(
                                        TypeError::RegisteredAsFunctionButNotFunction {
                                            name: name.clone(),
                                        },
                                    );
                                    None
                                }
                            }
                        }
                        _ => {
                            self.errors
                                .push(TypeError::ClosureNotSupported { name: name.clone() });
                            None
                        }
                    }
                } else {
                    self.errors
                        .push(TypeError::FunctionNotDefined { name: name.clone() });
                    None
                }
            }
            Expression::Tuple { values } => {
                let mut typed_values = vec![];
                let mut types = vec![];
                for value in values {
                    if let Some(typed_value) = self.infer_expression(value, env) {
                        types.push(typed_value.type_());
                        typed_values.push(typed_value);
                    }
                }
                Some(TypedExpression::Tuple {
                    values: typed_values,
                    type_: Rc::new(Type::Tuple { types }),
                })
            }
            Expression::TupleIndex { tuple, index } => {
                if let Some(typed_tuple) = self.infer_expression(tuple, env)
                    && let Some(TypedExpression::Integer { value, .. }) =
                        self.infer_expression(index, env)
                {
                    let type_ = typed_tuple
                        .type_()
                        .tuple_types()
                        .expect("Failed to index into non-tuple expression")
                        [value as usize]
                        .clone();
                    Some(TypedExpression::TupleIndex {
                        tuple: Box::new(typed_tuple),
                        index: value as u32,
                        type_,
                    })
                } else {
                    None
                }
            }
            Expression::Array { values } => {
                let mut typed_values = vec![];
                let mut element_type: Option<Rc<Type>> = None;
                for value in values {
                    let typed_value = self.infer_expression(value, env)?;
                    match &element_type {
                        Some(expected) if typed_value.type_() != *expected => {
                            self.errors.push(TypeError::ArrayElementTypeMismatch {
                                expected: expected.clone(),
                                found: typed_value.type_(),
                            });
                            return None;
                        }
                        Some(_) => {}
                        None => element_type = Some(typed_value.type_()),
                    }
                    typed_values.push(typed_value);
                }
                let Some(element_type) = element_type else {
                    self.errors.push(TypeError::EmptyArrayLiteral);
                    return None;
                };
                let size = typed_values.len() as u64;
                Some(TypedExpression::Array {
                    values: typed_values,
                    type_: Rc::new(Type::Array { element_type, size }),
                })
            }
            Expression::ArraySubscript { array, index } => {
                let typed_array = self.infer_expression(array, env)?;
                let typed_index = self.infer_expression(index, env)?;
                if *typed_index.type_() != Type::Int64 {
                    self.errors.push(TypeError::TypeMismatchArrayIndex {
                        found: typed_index.type_(),
                    });
                    return None;
                }
                let array_type = typed_array.type_();
                match array_type.as_ref() {
                    Type::Array { element_type, .. } => Some(TypedExpression::ArraySubscript {
                        type_: element_type.clone(),
                        array: Box::new(typed_array),
                        index: Box::new(typed_index),
                    }),
                    _ => {
                        self.errors.push(TypeError::SubscriptOnNonArray {
                            found: array_type.clone(),
                        });
                        None
                    }
                }
            }
            e => todo!("Type checking not implemented for {:#?}", e),
        }
    }
}

enum RegistrationStatus {
    Success,
    Failure,
}

#[derive(Clone, Debug)]
pub struct ValueConstructor {
    pub variant: ValueConstructorVariant,
    pub type_: Rc<Type>,
}

#[derive(Clone, Debug)]
pub enum ValueConstructorVariant {
    ModuleFunction { parameters: Vec<TypedParameter> },
    ModuleConstant,
    LocalVariable,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Fn {
        parameters: Vec<Rc<Type>>,
        return_type: Rc<Type>,
    },
    Tuple {
        types: Vec<Rc<Type>>,
    },
    Array {
        element_type: Rc<Type>,
        size: u64,
    },
    Int64,
    Float64,
    Bool,
    Unit,
}

impl Type {
    fn fn_types(&self) -> Option<(Vec<Rc<Type>>, Rc<Type>)> {
        match self {
            Type::Fn {
                parameters,
                return_type,
            } => Some((parameters.clone(), return_type.clone())),
            _ => None,
        }
    }

    pub fn tuple_types(&self) -> Option<Vec<Rc<Type>>> {
        match self {
            Type::Tuple { types } => Some(types.clone()),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    UnknownType,
    ArgumentNameAlreadyUsed {
        name: String,
    },
    VariableNotDefined {
        name: String,
    },
    FunctionNotDefined {
        name: String,
    },
    BinaryOpTypeMismatch {
        left: Rc<Type>,
        right: Rc<Type>,
    },
    ClosureNotSupported {
        name: String,
    },
    RegisteredAsFunctionButNotFunction {
        name: String,
    },
    UnexpectedNumberOfArguments {
        name: String,
    },
    UnexpectedTypeToFunction {
        name: String,
        parameter_name: String,
        expected: Rc<Type>,
        found: Rc<Type>,
    },
    TypeMismatchAssign {
        name: String,
        expected: Rc<Type>,
        found: Rc<Type>,
    },
    TypeMismatchIfCondition {
        found: Rc<Type>,
    },
    TypeMismatchWhileCondition {
        found: Rc<Type>,
    },
    TypeMismatchReturnValue {
        name: String,
        expected: Rc<Type>,
        found: Rc<Type>,
    },
    InvalidAssignmentTarget,
    ArrayElementTypeMismatch {
        expected: Rc<Type>,
        found: Rc<Type>,
    },
    EmptyArrayLiteral,
    TypeMismatchArrayIndex {
        found: Rc<Type>,
    },
    SubscriptOnNonArray {
        found: Rc<Type>,
    },
}

#[derive(Debug)]
pub struct TypedModule {
    pub declarations: Vec<TypedDeclaration>,
}

impl TypedModule {
    pub fn add_entry_point(&mut self) {
        let main_fn = self
            .declarations
            .iter_mut()
            .find_map(|declaration| match declaration {
                TypedDeclaration::Function(function) if function.name == "main" => Some(function),
                _ => None,
            });
        if let Some(function) = main_fn {
            match function.return_type.as_ref() {
                Type::Unit => {
                    function.name = "__notun_main".to_string();
                }
                _ => {
                    panic!("The main function only supports unit return type")
                }
            }
        }
        self.declarations
            .push(TypedDeclaration::Function(TypedFunction {
                name: "main".to_string(),
                parameters: vec![],
                return_type: Rc::new(Type::Int64),
                body: vec![
                    TypedStatement::Expression(TypedExpression::Call {
                        name: "__notun_main".to_string(),
                        arguments: vec![],
                        type_: Rc::new(Type::Unit),
                    }),
                    TypedStatement::Return {
                        return_value: TypedExpression::Integer {
                            value: 0,
                            type_: Rc::new(Type::Int64),
                        },
                    },
                ],
            }));
    }
}

#[derive(Debug)]
pub enum TypedDeclaration {
    Function(TypedFunction),
}

#[derive(Debug)]
pub struct TypedFunction {
    pub name: String,
    pub parameters: Vec<TypedParameter>,
    pub return_type: Rc<Type>,
    pub body: Vec<TypedStatement>,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub name: String,
    pub type_: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Let {
        name: String,
        type_: Rc<Type>,
        value: TypedExpression,
    },
    Assignment {
        lhs: TypedPlace,
        type_: Rc<Type>,
        rhs: TypedExpression,
    },
    Expression(TypedExpression),
    If {
        condition: TypedExpression,
        then_branch: Vec<Self>,
        else_branch: Option<Vec<Self>>,
    },
    While {
        condition: TypedExpression,
        body: Vec<Self>,
    },
    Return {
        return_value: TypedExpression,
    },
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    Integer {
        value: i64,
        type_: Rc<Type>,
    },
    Float {
        value: f64,
        type_: Rc<Type>,
    },
    Var {
        name: String,
        type_: Rc<Type>,
    },
    Bool {
        value: bool,
        type_: Rc<Type>,
    },
    BinaryOperation {
        operator: BinOp,
        left: Box<Self>,
        right: Box<Self>,
        type_: Rc<Type>,
    },
    NegateBang {
        value: Box<Self>,
        type_: Rc<Type>,
    },
    NegateMinus {
        value: Box<Self>,
        type_: Rc<Type>,
    },
    Positive {
        value: Box<Self>,
        type_: Rc<Type>,
    },
    Call {
        name: String,
        arguments: Vec<TypedExpression>,
        type_: Rc<Type>,
    },
    Tuple {
        values: Vec<TypedExpression>,
        type_: Rc<Type>,
    },
    Array {
        values: Vec<TypedExpression>,
        type_: Rc<Type>,
    },
    TupleIndex {
        tuple: Box<TypedExpression>,
        index: u32,
        type_: Rc<Type>,
    },
    ArraySubscript {
        array: Box<TypedExpression>,
        index: Box<TypedExpression>,
        type_: Rc<Type>,
    },
}

impl TypedExpression {
    pub fn type_(&self) -> Rc<Type> {
        let type_ = match self {
            TypedExpression::Integer { type_, .. } => type_,
            TypedExpression::Float { type_, .. } => type_,
            TypedExpression::Var { type_, .. } => type_,
            TypedExpression::Bool { type_, .. } => type_,
            TypedExpression::BinaryOperation { type_, .. } => type_,
            TypedExpression::NegateBang { type_, .. } => type_,
            TypedExpression::NegateMinus { type_, .. } => type_,
            TypedExpression::Positive { type_, .. } => type_,
            TypedExpression::Call { type_, .. } => type_,
            TypedExpression::Tuple { type_, .. } => type_,
            TypedExpression::Array { type_, .. } => type_,
            TypedExpression::TupleIndex { type_, .. } => type_,
            TypedExpression::ArraySubscript { type_, .. } => type_,
        };
        type_.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TypedPlace {
    pub name: String,
    pub root_type: Rc<Type>,
    pub path: Vec<u32>,
}
