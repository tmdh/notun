#[derive(Debug)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Function { name: String, body: Statement },
}

#[derive(Debug)]
pub enum Statement {
    Let {
        name: String,
        type_: String,
        value: Expression,
    },
    Block {
        statements: Vec<Statement>,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
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
    BinaryOperation {
        operator: BinOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}
