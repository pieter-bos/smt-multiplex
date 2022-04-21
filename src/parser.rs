use std::io::Read;
use std::result;
use crate::{lexer, Token, TokenReaderErr};
use crate::uninterpreted_ast::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Location {
    line_idx: u64,
    col_idx: u64,
}

enum ParseFailure {
    Recoverable(RecoverableParseError),
    Unrecoverable(UnrecoverableParseFailure),
}

enum RecoverableParseError {
    WrongStartToken { rule: &'static str, token: lexer::Token, loc: Location },
    NoAlt { rule: &'static str, alts: Vec<RecoverableParseError> },
    Context { rule: &'static str, inner: Box<RecoverableParseError> },
}

enum UnrecoverableParseFailure {
    Eof,
    Token(lexer::TokenReaderErr),
    Unexpected { rule: &'static str, token: lexer::Token, expected: &'static str, loc: Location },
    Context { rule: &'static str, loc: Location, inner: Box<UnrecoverableParseFailure> },
}

type Result<T> = result::Result<T, ParseFailure>;

struct Parser<R: Read> {
    lexer: lexer::TokenReader<R>,
}

impl <R: Read> Parser<R> {
    fn get_loc(&self) -> Location {
        Location {
            line_idx: self.lexer.get_line_idx(),
            col_idx: self.lexer.get_col_idx(),
        }
    }

    fn alts<T, F : FnOnce() -> Result<T>>(&self, rule: &'static str, alts: Vec<F>) -> Result<T> {
        let loc = self.get_loc();
        let mut alt_errs = Vec::<RecoverableParseError>::new();

        for f in alts {
            match f() {
                Ok(t) => { return Result::Ok(t); }
                Err(ParseFailure::Recoverable(rec_err)) => { alt_errs.push(rec_err); }
                Err(ParseFailure::Unrecoverable(unrec_err)) => {
                    return Result::Err(ParseFailure::Unrecoverable(UnrecoverableParseFailure::Context {
                        rule, loc, inner: Box::new(unrec_err),
                    }))
                }
            }
        }

        Result::Err(ParseFailure::Recoverable(RecoverableParseError::NoAlt { rule, alts: alt_errs }))
    }

    pub fn index(&mut self) -> Result<Identifier> {
        todo!()
    }
}