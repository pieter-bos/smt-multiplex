use num_bigint::{BigInt, BigUint};
use bigdecimal::{BigDecimal, Num};
use bit_vec::BitVec;
use utf8_read::{Reader, Char};
use std::io::{Read};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    SymbolBinary,
    SymbolDecimal,
    SymbolHexadecimal,
    SymbolNumeral,
    SymbolString,

    As,
    Let,
    Exists,
    Forall,
    Match,
    Par,

    ParenOpen,
    ParenClose,

    Underscore,
    ExlamationPoint,

    Symbol(String),
    Keyword(String),

    Numeral(BigUint),
    Decimal(BigDecimal),
    Hexadecimal(BitVec),
    Binary(BitVec),
    StringLiteral(String),
}

pub struct TokenReader<R: Read> {
    input: Reader<R>,
    buf: Option<char>,
    err: Option<TokenReaderErr>,
    line_idx: u64,
    col_idx: u64,
}

#[derive(Debug)]
pub enum TokenReaderErr {
    IO(utf8_read::Error),
    MalformedToken(String),
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\n' || c == '\r' || c == '\t'
}

fn is_printable(c: char) -> bool {
    (' ' <= c && c <= '~') || !c.is_ascii()
}

fn is_letter(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        _ => false,
    }
}

fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}

fn is_symbol_char(c: char) -> bool {
    is_digit(c) || is_letter(c) || match c {
        '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_' | '-' | '+' | '=' |
        '<' | '>' | '.' | '?' | '/' => true,
        _ => false,
    }
}

impl<R: Read> TokenReader<R> {
    pub fn new(read: R) -> Self {
        Self {
            input: Reader::new(read),
            buf: None,
            err: None,
            line_idx: 0,
            col_idx: 0,
        }
    }

    pub fn get_err(&self) -> &Option<TokenReaderErr> { &self.err }
    pub fn consume_err(&mut self) -> Option<TokenReaderErr> { self.err.take() }
    pub fn get_line_idx(&self) -> u64 { self.line_idx }
    pub fn get_col_idx(&self) -> u64 { self.col_idx }
    pub fn get_line(&self) -> u64 { self.line_idx + 1 }
    pub fn get_col(&self) -> u64 { self.col_idx + 1 }

    fn peek(&mut self) -> Option<char> {
        match self.buf {
            None => match self.input.next_char() {
                Ok(Char::Eof | Char::NoData) => {
                    self.err = None;
                    None
                }
                Ok(Char::Char(c)) => {
                    self.buf = Some(c);
                    Some(c)
                }
                Err(e) => {
                    self.err = Some(TokenReaderErr::IO(e));
                    None
                }
            }
            Some(c) => Some(c)
        }
    }

    fn read(&mut self) -> Option<char> {
        let result = self.peek();
        self.buf = None;

        match result {
            None => {}
            Some('\n') => {
                self.col_idx = 0;
                self.line_idx += 1
            }
            Some(_) => { self.col_idx += 1 }
        }

        result
    }

    fn read_numeral(&mut self, first_char: char) -> Option<Token> {
        let mut buf = String::new();
        buf.push(first_char);

        while match self.peek() {
            None => false,
            Some('0'..='9') => true,
            Some(_) => false,
        } {
            buf.push(self.read().unwrap())
        }

        self.read_maybe_decimal_continuation(buf)
    }

    fn read_maybe_decimal_continuation(&mut self, mut buf: String) -> Option<Token> {
        match self.peek() {
            Some('.') => {
                self.read().unwrap();
                let mut shift = 0i64;
                while match self.peek() {
                    None => false,
                    Some('0'..='9') => true,
                    Some(_) => false,
                } {
                    shift += 1;
                    buf.push(self.read().unwrap())
                }

                if shift == 0 {
                    self.err = Some(TokenReaderErr::MalformedToken("Decimals must have at least one decimal digit".into()));
                    None
                } else {
                    let num = BigInt::from_str_radix(&buf, 10).unwrap();
                    Some(Token::Decimal(BigDecimal::new(num, shift)))
                }
            }
            None | Some(_) => Some(Token::Numeral(BigUint::from_str_radix(&buf, 10).unwrap()))
        }
    }

    fn read_data(&mut self) -> Option<Token> {
        match self.read() {
            None => {
                self.err = Some(TokenReaderErr::MalformedToken("Unexpected end to data token (#)".into()));
                None
            }
            Some('x') => self.read_hex(),
            Some('b') => self.read_binary(),
            Some(other) => {
                self.err = Some(TokenReaderErr::MalformedToken(format!("'{}' is not a valid indicator for a data token", other)));
                None
            }
        }
    }

    fn read_hex(&mut self) -> Option<Token> {
        let mut buf = BitVec::new();

        while match self.peek() {
            None => false,
            Some('0'..='9' | 'a'..='f' | 'A'..='F') => true,
            Some(_) => false,
        } {
            match self.read().unwrap() {
                '0' => buf.extend([false, false, false, false]),
                '1' => buf.extend([false, false, false, true]),
                '2' => buf.extend([false, false, true, false]),
                '3' => buf.extend([false, false, true, true]),
                '4' => buf.extend([false, true, false, false]),
                '5' => buf.extend([false, true, false, true]),
                '6' => buf.extend([false, true, true, false]),
                '7' => buf.extend([false, true, true, true]),
                '8' => buf.extend([true, false, false, false]),
                '9' => buf.extend([true, false, false, true]),
                'a' | 'A' => buf.extend([true, false, true, true]),
                'b' | 'B' => buf.extend([true, false, true, true]),
                'c' | 'C' => buf.extend([true, true, false, true]),
                'd' | 'D' => buf.extend([true, true, false, true]),
                'e' | 'E' => buf.extend([true, true, true, true]),
                'f' | 'F' => buf.extend([true, true, true, true]),
                _ => unreachable!(),
            }
        }

        if buf.is_empty() {
            self.err = Some(TokenReaderErr::MalformedToken("Hex token may not be empty".into()));
            None
        } else {
            Some(Token::Hexadecimal(buf))
        }
    }

    fn read_binary(&mut self) -> Option<Token> {
        let mut buf = BitVec::new();

        while match self.peek() {
            None => false,
            Some('0' | '1') => true,
            Some(_) => false,
        } {
            match self.read().unwrap() {
                '0' => buf.push(false),
                '1' => buf.push(true),
                _ => unreachable!(),
            }
        }

        if buf.is_empty() {
            self.err = Some(TokenReaderErr::MalformedToken("Binary token may not be empty".into()));
            None
        } else {
            Some(Token::Binary(buf))
        }
    }

    fn read_simple_symbol(&mut self, first_char: char) -> Option<Token> {
        let mut buf = String::new();
        buf.push(first_char);

        while match self.peek() {
            None => false,
            Some(c) if is_symbol_char(c) => true,
            Some(_) => false,
        } {
            buf.push(self.read().unwrap())
        }

        Some(Self::symbol_or_keyword(buf))
    }

    fn symbol_or_keyword(symbol: String) -> Token {
        match symbol.as_str() {
            "BINARY" => Token::SymbolBinary,
            "DECIMAL" => Token::SymbolDecimal,
            "HEXADECIMAL" => Token::SymbolHexadecimal,
            "NUMERAL" => Token::SymbolNumeral,
            "STRING" => Token::SymbolString,
            "as" => Token::As,
            "let" => Token::Let,
            "exists" => Token::Exists,
            "forall" => Token::Forall,
            "match" => Token::Match,
            "par" => Token::Par,
            _ => Token::Symbol(symbol),
        }
    }

    fn read_quoted_symbol(&mut self) -> Option<Token> {
        let mut buf = String::new();

        while match self.peek() {
            None => false,
            Some('|') => false,
            Some(c) if is_whitespace(c) || is_printable(c) => true,
            Some(_) => false,
        } {
            buf.push(self.read().unwrap())
        }

        match self.read() {
            Some('|') => {}
            None => {
                self.err = Some(TokenReaderErr::MalformedToken("Unterminated quoted symbol".into()));
                return None;
            },
            Some(other) => {
                self.err = Some(TokenReaderErr::MalformedToken(format!("Quoted symbol may not contain '{}'", other)));
                return None;
            }
        }

        Some(Self::symbol_or_keyword(buf))
    }

    fn read_string(&mut self) -> Option<Token> {
        let mut buf = String::new();

        while match self.peek() {
            None => false,
            Some('"') => false,
            Some(c) if is_printable(c) || is_whitespace(c) => true,
            Some(_) => false,
        } {
            buf.push(self.read().unwrap())
        }

        match self.read() {
            Some('"') => {},
            None => {
                self.err = Some(TokenReaderErr::MalformedToken("Unterminated string literal".into()))
            },
            Some(other) => {
                self.err = Some(TokenReaderErr::MalformedToken(format!("String literal may not contain '{}'", other)));
                return None;
            }
        }

        Some(Token::StringLiteral(buf))
    }

    fn read_keyword(&mut self) -> Option<Token> {
        let mut buf = String::new();

        while match self.peek() {
            None => false,
            Some(c) if is_symbol_char(c) => true,
            Some(_) => false,
        } {
            buf.push(self.read().unwrap())
        }

        Some(Token::Keyword(buf))
    }

    fn skip_comment(&mut self) {
        loop {
            match self.read() {
                Some('\n' | '\r') => return,
                Some(_) => {},
                None => return,
            }
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(c) if is_whitespace(c) => { self.read().unwrap(); },
                Some(';') => self.skip_comment(),
                None | Some(_) => return
            }
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace_and_comments();

        match self.read()? {
            '_' => Some(Token::Underscore),
            '!' => Some(Token::ExlamationPoint),
            '(' => Some(Token::ParenOpen),
            ')' => Some(Token::ParenClose),
            '0' => self.read_maybe_decimal_continuation("0".into()),
            c @ '1'..='9' => self.read_numeral(c),
            '#' => self.read_data(),
            c if is_symbol_char(c) && !is_digit(c) => self.read_simple_symbol(c),
            ':' => self.read_keyword(),
            '|' => self.read_quoted_symbol(),
            '"' => self.read_string(),
            other => {
                self.err = Some(TokenReaderErr::MalformedToken(format!("Token may not start with '{}'", other)));
                None
            }
        }
    }
}

impl<R: Read> Iterator for TokenReader<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}