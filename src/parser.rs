/*
// TODO: Fix grammar definitions
Module := Declaration*
Declaration := Function // | GlobalDeclaration
Function := 'fn' Identifier Parameters ('->' Type)? FunctionBody
Parameters := '(' (Parameter (',' Parameter)* ','?)? ')'
Parameter := Identifier ':' Type
FunctionBody := Block // | '=' Expression
Type := Constructor | Tuple | Unit
Tuple := '(' Type (',' Type)* ')'
Unit := '(' ')'
Block := '{' Statement* '}'
Statement := Let | Block | If | While // | Assignment | Expression
Expression := Int | Float | Var | Bool | BinaryOperation | NegateBang | NegateMinus | Call | TupleIndex
TupleIndex := Expression '.' Get | Expression
Call := Identifier '(' (Expression (',' Expression)* ','?)? ')'
Let := 'let' Identifier ':' Type '=' Expression
If := 'if' Expression '{' ThenBranch '}' ('else' ElseBranch)?
While := 'while' Expression '{' Statement '}'

*/

use crate::{
    ast::{
        ArrayTypeAst, BinOp, Constant, ConstructorTypeAst, Declaration, Expression, Function,
        Module, Parameter, Statement, TupleTypeAst, TypeAst,
    },
    lexer::{LexError, LexResult, Token, TokenKind},
};

pub struct Parser<T: Iterator<Item = LexResult>> {
    tokens: T,
    tok0: Option<Token>,
    tok1: Option<Token>,
    lex_errors: Vec<LexError>,
}

#[derive(Debug)]
pub enum ParseError {
    ExpectedIdentifier,
    ExpectedToken {
        expected: TokenKind,
        found: Option<Token>,
    },
    ExpectedElseBlock,
    ExpectedExpression,
    ExpectedTypeIdentifier,
    UnitArrayError,
}

impl<T> Parser<T>
where
    T: Iterator<Item = LexResult>,
{
    pub fn new(lexer: T) -> Parser<T> {
        let mut parser = Parser {
            tokens: lexer,
            tok0: None,
            tok1: None,
            lex_errors: vec![],
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) -> Option<Token> {
        let next;
        loop {
            match self.tokens.next() {
                Some(Ok(Token {
                    kind: TokenKind::NewLine,
                    ..
                })) => {}
                Some(Ok(tok)) => {
                    next = Some(tok);
                    break;
                }
                Some(Err(err)) => {
                    self.lex_errors.push(err);
                    next = None;
                    break;
                }
                None => {
                    next = None;
                    break;
                }
            };
        }
        // println!("{:#?}", self.tok0);
        std::mem::replace(&mut self.tok0, std::mem::replace(&mut self.tok1, next))
    }

    pub fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut declarations = vec![];
        loop {
            match self.next_token() {
                Some(Token {
                    kind: TokenKind::Fn,
                    ..
                }) => {
                    let function = self.parse_function()?;
                    declarations.push(function);
                }
                Some(Token {
                    kind: TokenKind::Let,
                    ..
                }) => {
                    let global = self.parse_global_declaration()?;
                    declarations.push(global);
                }
                _ => break,
            }
        }
        Ok(Module { declarations })
    }

    fn parse_global_declaration(&mut self) -> Result<Declaration, ParseError> {
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let type_ = self
            .parse_type()?
            .ok_or(ParseError::ExpectedTypeIdentifier)?;
        self.expect_token(TokenKind::Equal)?;
        let value = self.parse_expression()?;
        Ok(Declaration::Constant(Constant {
            name: identifier,
            type_,
            value,
        }))
    }

    fn parse_function(&mut self) -> Result<Declaration, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::LeftParen)?;
        let mut parameters = vec![];
        while let Some(parameter) = self.parse_function_parameter()? {
            parameters.push(parameter);
            match self.tok0 {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    self.next_token();
                }
                _ => break,
            }
        }
        self.expect_token(TokenKind::RightParen)?;
        let mut return_type = None;
        match self.tok0 {
            Some(Token {
                kind: TokenKind::RightArrow,
                ..
            }) => {
                self.next_token();
                return_type = Some(
                    self.parse_type()?
                        .ok_or(ParseError::ExpectedTypeIdentifier)?,
                );
            }
            _ => {}
        }
        self.expect_token(TokenKind::LeftBrace)?;
        let body = self.parse_statements()?;
        self.expect_token(TokenKind::RightBrace)?;
        Ok(Declaration::Function(Function {
            name,
            parameters,
            body,
            return_type: return_type.unwrap_or(TypeAst::Unit),
        }))
    }

    fn parse_function_parameter(&mut self) -> Result<Option<Parameter>, ParseError> {
        match &self.tok0 {
            Some(Token {
                kind: TokenKind::Identifier { name },
                ..
            }) => {
                let name = name.clone();
                self.next_token();
                self.expect_token(TokenKind::Colon)?;
                let type_ = self
                    .parse_type()?
                    .ok_or(ParseError::ExpectedTypeIdentifier)?;
                Ok(Some(Parameter { name, type_ }))
            }
            _ => Ok(None),
        }
    }

    fn parse_type(&mut self) -> Result<Option<TypeAst>, ParseError> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Identifier { name },
                ..
            }) => {
                let mut type_ = TypeAst::Constructor(ConstructorTypeAst { name });
                while matches!(
                    self.tok0,
                    Some(Token {
                        kind: TokenKind::LeftSquare,
                        ..
                    })
                ) {
                    self.next_token();
                    let dimension = self.parse_expression()?;
                    self.expect_token(TokenKind::RightSquare)?;
                    match type_ {
                        TypeAst::Constructor(..) | TypeAst::Tuple(..) => {
                            type_ = TypeAst::Array(ArrayTypeAst {
                                type_: type_.into(),
                                dimensions: vec![dimension],
                            })
                        }
                        TypeAst::Array(mut array) => {
                            array.dimensions.push(dimension);
                            type_ = TypeAst::Array(array);
                        }
                        TypeAst::Unit => return Err(ParseError::UnitArrayError),
                    }
                }
                Ok(Some(type_))
            }
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                let mut types = vec![];
                while let Some(type_) = self.parse_type()? {
                    types.push(type_);
                    match self.tok0 {
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        }) => {
                            self.next_token();
                        }
                        _ => break,
                    }
                }
                self.expect_token(TokenKind::RightParen)?;
                let type_ = if types.len() == 0 {
                    TypeAst::Unit
                } else {
                    TypeAst::Tuple(TupleTypeAst { types })
                };
                Ok(Some(type_))
            }
            _ => Ok(None),
        }
    }

    fn parse_statements(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = vec![];
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
            match self.tok0 {
                Some(Token {
                    kind: TokenKind::RightBrace,
                    ..
                }) => break,
                _ => {}
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, ParseError> {
        match &self.tok0 {
            Some(Token {
                kind: TokenKind::Let,
                ..
            }) => {
                self.next_token();
                Ok(Some(self.parse_let_statement()?))
            }
            Some(Token {
                kind: TokenKind::If,
                ..
            }) => {
                self.next_token();
                Ok(Some(self.parse_if_statement()?))
            }
            Some(Token {
                kind: TokenKind::While,
                ..
            }) => {
                self.next_token();
                Ok(Some(self.parse_while_statement()?))
            }
            Some(Token {
                kind: TokenKind::Return,
                ..
            }) => {
                self.next_token();
                Ok(Some(self.parse_return_statement()?))
            }
            Some(Token {
                kind: TokenKind::Identifier { name },
                ..
            }) => {
                let name = name.clone();
                self.next_token();
                Ok(Some(self.parse_assignment_statement(name)?))
            }
            Some(c) => {
                if !c.kind.is_expression_start() {
                    panic!("A statement shouldn't start with {:#?}", c);
                }
                Ok(Some(Statement::Expression(self.parse_expression()?)))
            }
            _ => todo!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let type_ = self
            .parse_type()?
            .ok_or(ParseError::ExpectedTypeIdentifier)?;
        self.expect_token(TokenKind::Equal)?;
        let value = self.parse_expression()?;
        Ok(Statement::Let {
            name: identifier,
            type_,
            value,
        })
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::LeftBrace)?;
        let then_branch = self.parse_statements()?;
        self.expect_token(TokenKind::RightBrace)?;
        let mut else_branch = None;
        if let Some(Token {
            kind: TokenKind::Else,
            ..
        }) = self.tok0
        {
            self.next_token();
            self.expect_token(TokenKind::LeftBrace)?;
            else_branch = Some(self.parse_statements()?);
            self.expect_token(TokenKind::RightBrace)?;
        }
        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::LeftBrace)?;
        let body = self.parse_statements()?;
        self.expect_token(TokenKind::RightBrace)?;
        Ok(Statement::While { condition, body })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let return_value = self.parse_expression()?;
        Ok(Statement::Return { return_value })
    }

    fn parse_assignment_statement(&mut self, name: String) -> Result<Statement, ParseError> {
        self.expect_token(TokenKind::Equal)?;
        let rhs = self.parse_expression()?;
        Ok(Statement::Assignment { lhs: name, rhs })
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut arguments = vec![];
        loop {
            let argument = self.parse_expression()?;
            arguments.push(argument);
            match self.tok0 {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {
                    self.next_token();
                }
                _ => break,
            }
        }
        Ok(arguments)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_expression_bp(0)
    }

    fn parse_expression_bp(&mut self, min_bp: u8) -> Result<Expression, ParseError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let op = match &self.tok0 {
                Some(Token {
                    kind: TokenKind::Plus,
                    ..
                }) => BinOp::Add,
                Some(Token {
                    kind: TokenKind::Minus,
                    ..
                }) => BinOp::Subtract,
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => BinOp::Multiply,
                Some(Token {
                    kind: TokenKind::Slash,
                    ..
                }) => BinOp::Divide,
                Some(Token {
                    kind: TokenKind::Modulo,
                    ..
                }) => BinOp::Modulo,
                Some(Token {
                    kind: TokenKind::EqualEqual,
                    ..
                }) => BinOp::EqualEqual,
                Some(Token {
                    kind: TokenKind::NotEqual,
                    ..
                }) => BinOp::NotEqual,
                Some(Token {
                    kind: TokenKind::Less,
                    ..
                }) => BinOp::Less,
                Some(Token {
                    kind: TokenKind::LessEqual,
                    ..
                }) => BinOp::LessEqual,
                Some(Token {
                    kind: TokenKind::Greater,
                    ..
                }) => BinOp::Greater,
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    ..
                }) => BinOp::GreaterEqual,
                Some(Token {
                    kind: TokenKind::PipePipe,
                    ..
                }) => BinOp::LogicalOr,
                Some(Token {
                    kind: TokenKind::AmpersandAmpersand,
                    ..
                }) => BinOp::LogicalAnd,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => BinOp::Dot,
                Some(Token {
                    kind: TokenKind::LeftSquare,
                    ..
                }) => BinOp::ArraySubscript,
                _ => break,
            };

            if let Some((l_bp, _)) = self.postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.next_token();
                lhs = match op {
                    BinOp::ArraySubscript => {
                        let index = self.parse_expression()?;
                        self.expect_token(TokenKind::RightSquare)?;
                        Expression::ArraySubscript {
                            array: lhs.into(),
                            index: index.into(),
                        }
                    }
                    _ => panic!("Postfix {:#?} in expressions is not supported.", op),
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = self.infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.next_token();
                let rhs = self.parse_expression_bp(r_bp)?;
                lhs = match op {
                    BinOp::Dot => Expression::TupleIndex {
                        tuple: lhs.into(),
                        index: rhs.into(),
                    },
                    _ => Expression::BinaryOperation {
                        operator: op,
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    },
                };
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Integer { value },
                ..
            }) => Ok(Expression::Integer { value }),
            Some(Token {
                kind: TokenKind::Float { value },
                ..
            }) => Ok(Expression::Float { value }),
            Some(Token {
                kind: TokenKind::Identifier { name },
                ..
            }) => match self.tok0 {
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => {
                    self.next_token();
                    let arguments = self.parse_function_arguments()?;
                    self.expect_token(TokenKind::RightParen)?;
                    Ok(Expression::Call { name, arguments })
                }
                _ => Ok(Expression::Var { name }),
            },
            Some(Token {
                kind: TokenKind::True,
                ..
            }) => Ok(Expression::Bool { value: true }),
            Some(Token {
                kind: TokenKind::False,
                ..
            }) => Ok(Expression::Bool { value: false }),
            Some(Token {
                kind: TokenKind::LeftParen,
                ..
            }) => {
                let mut values = vec![];
                loop {
                    let value = self.parse_expression()?;
                    values.push(value);
                    if matches!(
                        self.tok0,
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        })
                    ) {
                        self.next_token();
                    } else {
                        break;
                    }
                }
                self.expect_token(TokenKind::RightParen)?;
                if values.len() == 1 {
                    Ok(values.pop().unwrap())
                } else {
                    Ok(Expression::Tuple { values })
                }
            }
            Some(Token {
                kind: TokenKind::LeftSquare,
                ..
            }) => {
                let mut values = vec![];
                loop {
                    let value = self.parse_expression()?;
                    values.push(value);
                    if matches!(
                        self.tok0,
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        })
                    ) {
                        self.next_token();
                    } else {
                        break;
                    }
                }
                self.expect_token(TokenKind::RightSquare)?;
                Ok(Expression::Array { values })
            }
            token @ Some(
                Token {
                    kind: TokenKind::Plus,
                    ..
                }
                | Token {
                    kind: TokenKind::Minus,
                    ..
                }
                | Token {
                    kind: TokenKind::Bang,
                    ..
                },
            ) => {
                let token = token.unwrap();

                let r_bp = if let Some(((), r_bp)) = self.prefix_binding_power(&token.kind) {
                    r_bp
                } else {
                    return Err(ParseError::ExpectedExpression);
                };
                let rhs = self.parse_expression_bp(r_bp)?;
                match token {
                    Token {
                        kind: TokenKind::Bang,
                        ..
                    } => Ok(Expression::NegateBang { value: rhs.into() }),
                    Token {
                        kind: TokenKind::Minus,
                        ..
                    } => Ok(Expression::NegateMinus { value: rhs.into() }),
                    Token {
                        kind: TokenKind::Plus,
                        ..
                    } => Ok(Expression::Positive { value: rhs.into() }),
                    _ => panic!("no other token expected"),
                }
            }
            _ => Err(ParseError::ExpectedExpression),
        }
    }

    fn prefix_binding_power(&self, kind: &TokenKind) -> Option<((), u8)> {
        use TokenKind::*;
        match kind {
            Bang | Plus | Minus => Some(((), 11)),
            _ => None,
        }
    }

    fn infix_binding_power(&self, op: &BinOp) -> Option<(u8, u8)> {
        let res = match op {
            BinOp::LogicalOr => (1, 2),
            BinOp::LogicalAnd => (3, 4),
            BinOp::EqualEqual
            | BinOp::NotEqual
            | BinOp::Less
            | BinOp::LessEqual
            | BinOp::Greater
            | BinOp::GreaterEqual => (5, 6),
            BinOp::Add | BinOp::Subtract => (7, 8),
            BinOp::Multiply | BinOp::Divide | BinOp::Modulo => (9, 10),
            BinOp::Dot => (16, 15),
            _ => return None,
        };
        Some(res)
    }

    fn postfix_binding_power(&self, op: &BinOp) -> Option<(u8, ())> {
        match op {
            BinOp::ArraySubscript => Some((13, ())),
            _ => None,
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Identifier { name },
                ..
            }) => Ok(name),
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    fn expect_token(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        match self.next_token() {
            Some(tok) if tok.kind == expected => Ok(()),
            found => Err(ParseError::ExpectedToken { expected, found }),
        }
    }
}
