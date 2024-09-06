#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Function {
        name: String,
        parameters: Vec<Parameter>,
        body: Statement,
        return_type: Type,
    },
}

#[derive(Debug)]
pub enum Statement {
    Let {
        name: String,
        type_: String,
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
    Call {
        name: String,
        arguments: Vec<Expression>,
    },
    Tuple {
        values: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum Type {
    Constructor(ConstructorType),
    Tuple(TupleType),
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
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}
