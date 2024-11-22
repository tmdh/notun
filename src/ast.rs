#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Function(Function),
    Constant(Constant),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeAst,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Constant {
    pub name: String,
    pub type_: TypeAst,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        name: String,
        type_: TypeAst,
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TypeAst {
    Constructor(ConstructorTypeAst),
    Tuple(TupleTypeAst),
    Array(ArrayTypeAst),
    Unit,
}

#[derive(Debug, Clone)]
pub struct ConstructorTypeAst {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TupleTypeAst {
    pub types: Vec<TypeAst>,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeAst {
    pub type_: Box<TypeAst>,
    pub dimensions: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_: TypeAst,
}
