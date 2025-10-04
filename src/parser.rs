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

use std::fmt;

use chumsky::prelude::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug)]
pub enum Token<'src> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    // LeftSquare,
    // RightSquare,

    // Plus,
    // Minus,
    // Star,
    // Slash,
    // Modulo,
    // PipePipe,
    // AmpersandAmpersand,
    // Less,
    // Greater,
    // LessEqual,
    // GreaterEqual,
    // Equal,
    // EqualEqual,
    // NotEqual,
    // Bang,
    // Colon,
    RightArrow,
    // Dot,
    // Comma,
    Fn,
    // Let,
    // If,
    // Else,
    Return,
    // While,
    // True,
    // False,

    // NewLine,
    // EndOfFile,
    Integer(i64),
    // Float(f64),
    Identifier(&'src str),
    // Str(&'src str),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Fn => write!(f, "fn"),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Return => write!(f, "return"),
            Token::RightArrow => write!(f, "->"),
            Token::Integer(s) => write!(f, "{s}"),
        }
    }
}

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Integer);

    let op = just("(")
        .to(Token::LeftParen)
        .or(just(")").to(Token::RightParen))
        .or(just("{").to(Token::LeftBrace))
        .or(just("}").to(Token::RightBrace))
        .or(just("->").to(Token::RightArrow));

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "fn" => Token::Fn,
        "return" => Token::Return,
        _ => Token::Identifier(ident),
    });

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let token = num.or(op).or(ident);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
