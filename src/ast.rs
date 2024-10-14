#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, Copy)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        location: SrcSpan,
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Statement,
    },
    Global {
        location: SrcSpan,
        name: String,
        type_: Type,
        value: Expression,
    },
}

#[derive(Debug)]
pub enum Statement {
    Let {
        location: SrcSpan,
        name: String,
        type_: Type,
        value: Expression,
    },
    Assignment {
        location: SrcSpan,
        lhs: Expression,
        rhs: Expression,
    },
    Expression(Expression),
    Block {
        location: SrcSpan,
        statements: Vec<Self>,
    },
    If {
        location: SrcSpan,
        condition: Expression,
        then_branch: Box<Self>,
        else_branch: Option<Box<Self>>,
    },
    While {
        location: SrcSpan,
        condition: Expression,
        body: Box<Self>,
    },
    Return {
        location: SrcSpan,
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
        location: SrcSpan,
        value: i64,
    },
    Float {
        location: SrcSpan,
        value: f64,
    },
    Var {
        location: SrcSpan,
        name: String,
    },
    Bool {
        location: SrcSpan,
        value: bool,
    },
    BinaryOperation {
        location: SrcSpan,
        operator: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    NegateBang {
        location: SrcSpan,
        value: Box<Self>,
    },
    NegateMinus {
        location: SrcSpan,
        value: Box<Self>,
    },
    Positive {
        location: SrcSpan,
        value: Box<Self>,
    },
    Call {
        location: SrcSpan,
        name: String,
        arguments: Vec<Expression>,
    },
    Tuple {
        location: SrcSpan,
        values: Vec<Expression>,
    },
    Array {
        location: SrcSpan,
        values: Vec<Expression>,
    },
    TupleIndex {
        location: SrcSpan,
        tuple: Box<Expression>,
        index: Box<Expression>,
    },
    ArraySubscript {
        location: SrcSpan,
        array: Box<Expression>,
        index: Box<Expression>,
    },
}

impl Expression {
    pub fn location(&self) -> SrcSpan {
        use Expression::*;
        match self {
            Integer { location, .. }
            | Float { location, .. }
            | Var { location, .. }
            | Bool { location, .. }
            | BinaryOperation { location, .. }
            | NegateBang { location, .. }
            | NegateMinus { location, .. }
            | Positive { location, .. }
            | Call { location, .. }
            | Tuple { location, .. }
            | Array { location, .. }
            | TupleIndex { location, .. }
            | ArraySubscript { location, .. } => *location,
        }
    }
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
