pub enum IrExpression {
    Const(Literal),
    Location(IrLocationExpression),
    Operation {
        op: IrBinaryOperation,
        left: Box<Self>,
        right: Box<Self>,
    },
    Call {
        function: Name,
        parameters: Vec<Self>,
    },
    Name(Name),
    Eseq {
        statements: Box<IrStatement>,
        expression: Box<Self>,
    },
}

pub enum IrBinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    FAdd,
    FSub,
    FMul,
    FDiv,
    Mod,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    And, // bitwise
    Or,
    Not,
}

pub enum IrStatement {
    Move {
        destination: Box<IrLocationExpression>,
        source: Box<IrExpression>,
    },
    Seq(Vec<Self>),
    Jump(Box<IrExpression>),
    CJump {
        condition: Box<IrExpression>,
        nonzero: Label,
        zero: Label,
    },
    Label(Label),
    Return(Box<IrExpression>),
}

pub struct Label(&'static str);

pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

pub struct Name(u32);

pub enum IrLocationExpression {
    Temp(u32),
    Mem(u64),
}
