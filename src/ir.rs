use std::fmt;
use std::rc::Rc;
use crate::type_checker::{TypedModule, TypedFunction, TypedStatement, TypedExpression, Type, TypedDeclaration};

#[derive(Debug, Clone)]
pub enum Const {
    Int64(i64),
    Float64(f64)
}

#[derive(Debug, Clone)]
pub struct Temp(pub String);

#[derive(Debug, Clone)]
pub struct Label(pub String);

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Const(Const),
    Temp(Temp),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    Mem(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Name(Label),
    ESeq(Box<Stmt>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Move(Box<Expr>, Box<Expr>),
    Seq(Vec<Stmt>),
    Jump(Box<Expr>),
    CJump(Box<Expr>, Label, Label),
    Label(Label),
    Return(Box<Expr>),
}

pub struct LabelGenerator {
    counter: usize,
    prefix: String,
}

impl LabelGenerator {
    pub fn new(prefix: &str) -> Self {
        Self {
            counter: 0,
            prefix: prefix.to_string(),
        }
    }
    
    pub fn next(&mut self) -> Label {
        let label = Label(format!("{}_{}", self.prefix, self.counter));
        self.counter += 1;
        label
    }
}

pub struct TempGenerator {
    counter: usize,
    prefix: String,
}

impl TempGenerator {
    pub fn new(prefix: &str) -> Self {
        Self {
            counter: 0,
            prefix: prefix.to_string(),
        }
    }
    
    pub fn next(&mut self) -> Temp {
        let temp = Temp(format!("{}_{}", self.prefix, self.counter));
        self.counter += 1;
        temp
    }
    
    pub fn return_value(&mut self, index: usize) -> Temp {
        Temp(format!("RV{}", index))
    }
}

pub struct IrContext {
    pub label_gen: LabelGenerator,
    pub temp_gen: TempGenerator,
}

impl IrContext {
    pub fn new() -> Self {
        Self {
            label_gen: LabelGenerator::new("L"),
            temp_gen: TempGenerator::new("t"),
        }
    }
    
    pub fn fresh_label(&mut self) -> Label {
        self.label_gen.next()
    }
    
    pub fn fresh_temp(&mut self) -> Temp {
        self.temp_gen.next()
    }
}

pub struct Translator {
    pub context: IrContext,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            context: IrContext::new(),
        }
    }

    pub fn translate_module(&mut self, module: &TypedModule) -> Vec<Stmt> {
        let mut result = Vec::new();
        
        for decl in &module.declarations {
            match decl {
                TypedDeclaration::Function(func) => result.push(self.translate_function(func))
            }
        }
        
        result
    }

    pub fn translate_function(&mut self, func: &TypedFunction) -> Stmt {
        let func_label = Label(func.name.clone());
        let mut body_stmts = Vec::new();
        
        for stmt in &func.body {
            let ir_stmt = self.translate_statement(stmt);
            body_stmts.push(ir_stmt);
        }
        
        Stmt::Seq(vec![
            Stmt::Label(func_label),
            Stmt::Seq(body_stmts),
        ])
    }

    fn translate_statement(&mut self, stmt: &TypedStatement) -> Stmt {
        match stmt {
            TypedStatement::Let { name, type_, value } => {
                let temp = Temp(name.clone());
                let expr = self.translate_expression(value);
                Stmt::Move(
                    Box::new(Expr::Temp(temp)),
                    Box::new(expr),
                )
            }
            
            TypedStatement::Assignment { lhs, type_, rhs } => {
                let temp = Temp(lhs.clone());
                let expr = self.translate_expression(rhs);
                Stmt::Move(
                    Box::new(Expr::Temp(temp)),
                    Box::new(expr),
                )
            }
            
            TypedStatement::Expression(expr) => {
                let ir_expr = self.translate_expression(expr);
                let temp = self.context.fresh_temp();
                Stmt::Move(
                    Box::new(Expr::Temp(temp)),
                    Box::new(ir_expr)
                )
            }
            
            TypedStatement::If { condition, then_branch, else_branch } => {
                let cond_expr = self.translate_expression(condition);
                
                let then_label = self.context.fresh_label();
                let else_label = self.context.fresh_label();
                let end_label = self.context.fresh_label();
                
                let mut then_stmts = Vec::new();
                for stmt in then_branch {
                    let ir_stmt = self.translate_statement(stmt);
                    then_stmts.push(ir_stmt);
                }
                
                let mut if_stmt = vec![
                    Stmt::CJump(Box::new(cond_expr), then_label.clone(), else_label.clone()),
                    Stmt::Label(then_label),
                    Stmt::Seq(then_stmts),
                    Stmt::Jump(Box::new(Expr::Name(end_label.clone()))),
                    Stmt::Label(else_label),
                ];
                
                if let Some(else_branch) = else_branch {
                    let mut else_stmts = Vec::new();
                    for stmt in else_branch {
                        let ir_stmt = self.translate_statement(stmt);
                        else_stmts.push(ir_stmt);
                    }
                    if_stmt.push(Stmt::Seq(else_stmts));
                }
                
                if_stmt.push(Stmt::Label(end_label));
                
                Stmt::Seq(if_stmt)
            }
            
            TypedStatement::While { condition, body } => {
                let test_label = self.context.fresh_label();
                let body_label = self.context.fresh_label();
                let end_label = self.context.fresh_label();
                
                let cond_expr = self.translate_expression(condition);
                
                let mut body_stmts = Vec::new();
                for stmt in body {
                    let ir_stmt = self.translate_statement(stmt);
                    body_stmts.push(ir_stmt);
                }
                
                Stmt::Seq(vec![
                    Stmt::Label(test_label.clone()),
                    Stmt::CJump(Box::new(cond_expr), body_label.clone(), end_label.clone()),
                    Stmt::Label(body_label),
                    Stmt::Seq(body_stmts),
                    Stmt::Jump(Box::new(Expr::Name(test_label))),
                    Stmt::Label(end_label),
                ])
            }
            
            TypedStatement::Return { return_value } => {
                let expr = self.translate_expression(return_value);
                Stmt::Return(Box::new(expr))
            }
        }
    }

    fn translate_expression(&mut self, expr: &TypedExpression) -> Expr {
        match expr {
            TypedExpression::Integer { value, type_ } => Expr::Const(Const::Int64(*value)),
            
            TypedExpression::Float { value, type_ } => Expr::Const(Const::Float64(*value)),
            
            TypedExpression::Bool { value, type_ } => Expr::Const(Const::Int64(if *value { 1 } else { 0 })),
            
            TypedExpression::Var { name, type_ } => Expr::Temp(Temp(name.clone())),
            
            TypedExpression::BinaryOperation { operator, left, right, type_ } => {
                use crate::ast;
                let left_expr = self.translate_expression(left);
                let right_expr = self.translate_expression(right);
                
                let ir_op = match operator {
                    ast::BinOp::Add => BinOp::Add,
                    ast::BinOp::Subtract => BinOp::Sub,
                    ast::BinOp::Multiply => BinOp::Mul,
                    ast::BinOp::Divide => BinOp::Div,
                    ast::BinOp::Modulo => BinOp::Mod,
                    ast::BinOp::Less => BinOp::Lt,
                    ast::BinOp::Greater => BinOp::Gt,
                    ast::BinOp::LessEqual => BinOp::Leq,
                    ast::BinOp::GreaterEqual => BinOp::Geq,
                    ast::BinOp::EqualEqual => BinOp::Eq,
                    ast::BinOp::NotEqual => BinOp::Neq,
                    ast::BinOp::Dot => unimplemented!(),
                    ast::BinOp::ArraySubscript => unimplemented!(),
                    ast::BinOp::LogicalAnd => unimplemented!(),
                    ast::BinOp::LogicalOr => unimplemented!(),
                };
                
                Expr::BinOp(ir_op, Box::new(left_expr), Box::new(right_expr))
            }
            
            TypedExpression::NegateBang { value, type_ } => {
                let value_expr = self.translate_expression(value);
                Expr::UnOp(UnOp::Not, Box::new(value_expr))
            }
            
            TypedExpression::NegateMinus { value, type_ } => {
                let value_expr = self.translate_expression(value);
                Expr::BinOp(
                    BinOp::Sub,
                    Box::new(Expr::Const(Const::Int64(0))),
                    Box::new(value_expr),
                )
            }
            
            TypedExpression::Positive { value, type_ } => {
                self.translate_expression(value)
            }
            
            TypedExpression::Call { name, arguments, type_ } => {
                let mut ir_args = Vec::new();
                for arg in arguments {
                    ir_args.push(self.translate_expression(arg));
                }
                Expr::Call(
                    Box::new(Expr::Name(Label(name.clone()))),
                    ir_args,
                )
            }
            
            TypedExpression::Tuple { values, type_ } => Expr::Const(Const::Int64(0)),
            
            TypedExpression::Array { values, type_ } => unimplemented!(),
            
            TypedExpression::TupleIndex { tuple, index, type_ } => unimplemented!(),
            
            TypedExpression::ArraySubscript { array, index, type_ } => unimplemented!(),
        }
    }
}
