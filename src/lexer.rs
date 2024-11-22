use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,

    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    PipePipe,
    AmpersandAmpersand,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    EqualEqual,
    NotEqual,
    Bang,
    Colon,
    RightArrow,
    Dot,
    Comma,

    Fn,
    Let,
    If,
    Else,
    Return,
    While,
    True,
    False,

    NewLine,
    EndOfFile,

    Integer { value: i64 },
    Float { value: f64 },
    Identifier { name: String },
}

impl TokenKind {
    fn from_keyword(keyword: &str) -> Option<TokenKind> {
        match keyword {
            "fn" => Some(TokenKind::Fn),
            "let" => Some(TokenKind::Let),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "return" => Some(TokenKind::Return),
            "while" => Some(TokenKind::While),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            _ => None,
        }
    }

    pub fn is_expression_start(&self) -> bool {
        use TokenKind::*;
        match self {
            Integer { .. }
            | Float { .. }
            | Identifier { .. }
            | True
            | False
            | Plus
            | Minus
            | Bang => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    start: u32,
    end: u32,
}

pub struct NewlineHandler<T: Iterator<Item = (u32, char)>> {
    source: T,
    chr0: Option<(u32, char)>,
    chr1: Option<(u32, char)>,
}

impl<T> NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    pub fn new(source: T) -> Self {
        let mut nlh = NewlineHandler {
            source,
            chr0: None,
            chr1: None,
        };
        let _ = nlh.shift();
        let _ = nlh.shift();
        nlh
    }

    fn shift(&mut self) -> Option<(u32, char)> {
        let result = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = self.source.next();
        result
    }
}

impl<T> Iterator for NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = (u32, char);

    fn next(&mut self) -> Option<Self::Item> {
        // Collapse \r\n into \n
        if let Some((i, '\r')) = self.chr0 {
            if let Some((_, '\n')) = self.chr1 {
                // Transform windows EOL into \n
                let _ = self.shift();
                // using the position from the \r
                self.chr0 = Some((i, '\n'))
            } else {
                // Transform MAC EOL into \n
                self.chr0 = Some((i, '\n'))
            }
        }

        self.shift()
    }
}

pub struct Lexer<T: Iterator<Item = (u32, char)>> {
    chars: T,
    pending: Vec<Token>,
    chr0: Option<char>,
    chr1: Option<char>,
    loc0: u32,
    loc1: u32,
}

#[derive(Debug)]
pub enum LexError {
    MultipleDecimalPointError,
    ParseFloatError(ParseFloatError),
    ParseIntegerError(ParseIntError),
    UnrecognizedTokenError(u32, char),
}

pub type LexResult = Result<Token, LexError>;

impl<T> Lexer<T>
where
    T: Iterator<Item = (u32, char)>,
{
    fn new(chars: T) -> Lexer<T> {
        let mut lexer = Lexer {
            chars,
            pending: vec![],
            chr0: None,
            chr1: None,
            loc0: 0,
            loc1: 0,
        };
        lexer.next_char();
        lexer.next_char();
        lexer
    }

    fn next_char(&mut self) -> Option<char> {
        let first = self.chr0;
        self.chr0 = self.chr1;
        self.chr1 = match self.chars.next() {
            Some((loc, c)) => {
                self.loc0 = self.loc1;
                self.loc1 = loc;
                Some(c)
            }
            None => {
                self.loc0 = self.loc1;
                self.loc1 += 1;
                None
            }
        };
        first
    }

    fn inner_next(&mut self) -> LexResult {
        while self.pending.is_empty() {
            self.consume()?;
        }
        Ok(self.pending.remove(0))
    }

    fn consume(&mut self) -> Result<(), LexError> {
        // Check if there is any character to lex
        if let Some(c) = self.chr0 {
            let mut check_for_minus = false;
            if is_identifier_start(c) {
                check_for_minus = true;
                let token = self.lex_identifier()?;
                self.add_token(token);
                self.lex_maybe_dot_access()?;
            } else if is_number_start(c, self.chr1) {
                check_for_minus = true;
                let token = self.lex_number(true)?;
                self.add_token(token);
            } else {
                self.consume_character(c)?;
            }
            if check_for_minus {
                if self.chr0 == Some('-') && is_number_start('-', self.chr1) {
                    self.eat_single_char(TokenKind::Minus);
                }
            }
        } else {
            // We have reached end of file.
            let token_pos = self.get_pos();
            self.add_token(Token {
                kind: TokenKind::EndOfFile,
                start: token_pos,
                end: token_pos,
            });
        }
        Ok(())
    }

    fn get_pos(&self) -> u32 {
        self.loc0
    }

    fn add_token(&mut self, token: Token) {
        self.pending.push(token);
    }

    fn lex_identifier(&mut self) -> LexResult {
        let mut name = String::new();
        let start = self.get_pos();

        while self
            .chr0
            .map(|c| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
            .unwrap_or(false)
        {
            name.push(self.next_char().unwrap());
        }

        let end = self.get_pos();

        if let Some(kind) = TokenKind::from_keyword(&name) {
            Ok(Token { kind, start, end })
        } else {
            Ok(Token {
                kind: TokenKind::Identifier { name },
                start,
                end,
            })
        }
    }

    fn lex_number(&mut self, maybe_float: bool) -> LexResult {
        let start = self.get_pos();
        let mut value = String::new();

        let mut found_dot = false;

        if self.chr0 == Some('-') {
            value.push(self.next_char().unwrap());
        }

        while matches!(self.chr0, Some('0'..='9')) {
            value.push(self.next_char().unwrap());
        }

        if !maybe_float {
            let end = self.get_pos();
            return Ok(Token {
                kind: TokenKind::Integer {
                    value: value.parse().map_err(|e| LexError::ParseIntegerError(e))?,
                },
                start,
                end,
            });
        }

        if self.chr0 == Some('.') {
            found_dot = true;
            value.push(self.next_char().unwrap());
        }

        while matches!(self.chr0, Some('0'..='9')) {
            value.push(self.next_char().unwrap());
        }

        let end = self.get_pos();

        if self.chr0 == Some('.') {
            Err(LexError::MultipleDecimalPointError)
        } else if found_dot {
            Ok(Token {
                kind: TokenKind::Float {
                    value: value.parse().map_err(|e| LexError::ParseFloatError(e))?,
                },
                start,
                end,
            })
        } else {
            Ok(Token {
                kind: TokenKind::Integer {
                    value: value.parse().map_err(|e| LexError::ParseIntegerError(e))?,
                },
                start,
                end,
            })
        }
    }

    fn lex_maybe_dot_access(&mut self) -> Result<(), LexError> {
        loop {
            if self.chr0 == Some('.') && matches!(self.chr1, Some('0'..='9')) {
                self.eat_single_char(TokenKind::Dot);
                let token = self.lex_number(false)?;
                self.add_token(token);
            } else {
                break;
            }
        }
        Ok(())
    }

    fn consume_character(&mut self, c: char) -> Result<(), LexError> {
        match c {
            '(' => self.eat_single_char(TokenKind::LeftParen),
            ')' => {
                self.eat_single_char(TokenKind::RightParen);
                self.lex_maybe_dot_access()?;
            }
            '{' => self.eat_single_char(TokenKind::LeftBrace),
            '}' => self.eat_single_char(TokenKind::RightBrace),
            '[' => self.eat_single_char(TokenKind::LeftSquare),
            ']' => {
                self.eat_single_char(TokenKind::RightSquare);
                self.lex_maybe_dot_access()?;
            }
            '+' => self.eat_single_char(TokenKind::Plus),
            '*' => self.eat_single_char(TokenKind::Star),
            '/' => self.eat_single_char(TokenKind::Slash),
            '%' => self.eat_single_char(TokenKind::Modulo),
            ':' => self.eat_single_char(TokenKind::Colon),
            '.' => self.eat_single_char(TokenKind::Dot),
            ',' => self.eat_single_char(TokenKind::Comma),
            '-' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('>') => {
                        self.next_char();
                        TokenKind::RightArrow
                    }
                    _ => TokenKind::Minus,
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '<' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('=') => {
                        self.next_char();
                        TokenKind::LessEqual
                    }
                    _ => TokenKind::Less,
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '>' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('=') => {
                        self.next_char();
                        TokenKind::GreaterEqual
                    }
                    _ => TokenKind::Greater,
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '=' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('=') => {
                        self.next_char();
                        TokenKind::EqualEqual
                    }
                    _ => TokenKind::Equal,
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '!' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('=') => {
                        self.next_char();
                        TokenKind::NotEqual
                    }
                    _ => TokenKind::Bang,
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            // lex logical or and operators, not pipe and ampersand
            '|' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('|') => {
                        self.next_char();
                        TokenKind::PipePipe
                    }
                    _ => return Err(LexError::UnrecognizedTokenError(self.get_pos(), '|')),
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '&' => {
                let start = self.get_pos();
                self.next_char();
                let kind = match self.chr0 {
                    Some('&') => {
                        self.next_char();
                        TokenKind::AmpersandAmpersand
                    }
                    _ => return Err(LexError::UnrecognizedTokenError(self.get_pos(), '&')),
                };
                let end = self.get_pos();
                self.add_token(Token { kind, start, end });
            }
            '\n' | ' ' | '\t' | '\r' => {
                let start = self.get_pos();
                self.next_char();
                let end = self.get_pos();
                if c == '\n' {
                    self.add_token(Token {
                        kind: TokenKind::NewLine,
                        start,
                        end,
                    });
                }
            }
            _ => return Err(LexError::UnrecognizedTokenError(self.get_pos(), ' ')),
        }
        Ok(())
    }

    fn eat_single_char(&mut self, kind: TokenKind) {
        let start = self.get_pos();
        self.next_char();
        let end = self.get_pos();
        self.add_token(Token { kind, start, end });
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = LexResult;
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner_next() {
            Ok(Token {
                kind: TokenKind::EndOfFile,
                ..
            }) => None,
            r => Some(r),
        }
    }
}

pub fn make_tokenizer(source: &str) -> impl Iterator<Item = LexResult> + '_ {
    let chars = source.char_indices().map(|(i, c)| (i as u32, c));
    let nlh = NewlineHandler::new(chars);
    Lexer::new(nlh)
}

fn is_identifier_start(c: char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z')
}

fn is_number_start(c0: char, c1: Option<char>) -> bool {
    match c0 {
        '0'..='9' => true,
        '-' => matches!(c1, Some('0'..='9')),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let source = "fn main() {
    let a : i64 = 1 + 2 * 3 + 5
    let b : i64 = 50 * -99
    print(a + b)
}";
        let tokenizer = make_tokenizer(source);
        let tokens: Vec<LexResult> = tokenizer.collect();
        println!("{:#?}", tokens);
    }
}
