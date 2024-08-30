/*
fn main() {
    let a : i64 = 5
    let b : i64 = 6
    print(a)
}

Module := Declaration*
Declaration := FunctionDeclaration // | GlobalDeclaration
FunctionDeclaration := 'fn' Identifier FunctionParameters ('->' Type)? FunctionBody
FunctionParameters := '(' (FunctionParameter (',' FunctionParameter)* ','?)? ')'
FunctionBody := Block // | '=' Expression
Block := '{' Statement* '}'
Statement := Let | Block | If | While // | Assignment | Expression
If := 'if' Expression '{' ThenBranch '}' ('else' ElseBranch)?
While := 'while' Expression '{' Statement '}'

TODO:
- Unary negate and bang
- Function parameter and return type
- Tuple type
- Function call
- Array constructor [1, 2, 3]
- Multidimensional arrays
- Array subscript index
- Global variable declaration
- Return statement
- Statement := Assignment | Expression
*/

use crate::{
    ast::{BinOp, Declaration, Expression, Module, Statement},
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
    ExpectedToken(TokenKind),
    ExpectedElseBlock,
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
                _ => break,
            }
        }
        Ok(Module { declarations })
    }

    fn parse_function(&mut self) -> Result<Declaration, ParseError> {
        let name = self.expect_identifier()?;
        self.expect_token(TokenKind::LeftParen)?;
        // TODO: parse arguments
        self.expect_token(TokenKind::RightParen)?;
        // TODO: parse return type
        self.expect_token(TokenKind::LeftBrace)?;
        let body = self.parse_block()?;
        self.expect_token(TokenKind::RightBrace)?;
        Ok(Declaration::Function { name, body })
    }

    fn parse_block(&mut self) -> Result<Statement, ParseError> {
        let mut statements = vec![];
        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }
        Ok(Statement::Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>, ParseError> {
        match self.tok0 {
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
            _ => Ok(None),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let identifier = self.expect_identifier()?;
        self.expect_token(TokenKind::Colon)?;
        let type_ = self.expect_identifier()?;
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
        let then_branch = self.parse_block()?;
        self.expect_token(TokenKind::RightBrace)?;
        let mut else_branch = None;
        if let Some(Token {
            kind: TokenKind::Else,
            ..
        }) = self.tok0
        {
            self.next_token();
            if let Some(branch) = self.parse_statement()? {
                else_branch = Some(Box::new(branch));
            } else {
                return Err(ParseError::ExpectedElseBlock);
            }
        }
        Ok(Statement::If {
            condition,
            then_branch: then_branch.into(),
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        let condition = self.parse_expression()?;
        self.expect_token(TokenKind::LeftBrace)?;
        let body = self.parse_block()?;
        self.expect_token(TokenKind::RightBrace)?;
        Ok(Statement::While {
            condition,
            body: body.into(),
        })
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
                _ => break,
            };

            let (l_bp, r_bp) = self.infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            self.next_token();

            let rhs = self.parse_expression_bp(r_bp)?;

            lhs = Expression::BinaryOperation {
                operator: op,
                left: Box::new(lhs),
                right: Box::new(rhs),
            };
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
                kind: TokenKind::Identifier { value },
                ..
            }) => Ok(Expression::Var { name: value }),
            _ => Err(ParseError::ExpectedToken(TokenKind::Integer { value: 0 })),
        }
    }

    fn infix_binding_power(&self, op: &BinOp) -> (u8, u8) {
        match op {
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
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        match self.next_token() {
            Some(Token {
                kind: TokenKind::Identifier { value },
                ..
            }) => Ok(value),
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    fn expect_token(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        match self.next_token() {
            Some(tok) if tok.kind == token_kind => Ok(()),
            _ => Err(ParseError::ExpectedToken(token_kind)),
        }
    }
}
