#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Statement,
    },
    Global {
        name: String,
        type_: Type,
        value: Expression,
    },
}

#[derive(Debug)]
pub enum Statement {
    Let {
        name: String,
        type_: Type,
        value: Expression,
    },
    Assignment {
        lhs: Expression,
        rhs: Expression,
    },
    Expression(Expression),
    Block {
        statements: Vec<Self>,
    },
    If {
        condition: Expression,
        then_branch: Box<Self>,
        else_branch: Option<Box<Self>>,
    },
    While {
        condition: Expression,
        body: Box<Self>,
    },
    Return {
        return_value: Expression,
    },
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LogicalOr,
    LogicalAnd,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    NotEqual,
    Dot,
    ArraySubscript,
}

#[derive(Debug)]
pub enum Expression {
    Integer {
        value: i64,
    },
    Float {
        value: f64,
    },
    Var {
        name: String,
    },
    Bool {
        value: bool,
    },
    BinaryOperation {
        operator: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    NegateBang {
        value: Box<Self>,
    },
    NegateMinus {
        value: Box<Self>,
    },
    Positive {
        value: Box<Self>,
    },
    Call {
        name: String,
        arguments: Vec<Expression>,
    },
    Tuple {
        values: Vec<Expression>,
    },
    Array {
        values: Vec<Expression>,
    },
    TupleIndex {
        tuple: Box<Expression>,
        index: Box<Expression>,
    },
    ArraySubscript {
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum Type {
    Constructor(ConstructorType),
    Tuple(TupleType),
    Array(ArrayType),
    Unit,
}

#[derive(Debug)]
pub struct ConstructorType {
    pub name: String,
}

#[derive(Debug)]
pub struct TupleType {
    pub types: Vec<Type>,
}

#[derive(Debug)]
pub struct ArrayType {
    pub type_: Box<Type>,
    pub dimensions: Vec<Expression>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}
