use std::fmt::{Debug, Display, Formatter, Write};
use crate::{lexer, Location, RecoverableParseError, Token, TokenReaderErr, UnrecoverableParseFailure};
use crate::uninterpreted_ast::ScriptCommand;

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line_idx+1, self.col_idx+1)
    }
}

impl Display for TokenReaderErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenReaderErr::IO(err) => write!(f, "{}", err),
            TokenReaderErr::MalformedToken(err) => f.write_str(err),
        }
    }
}

impl Display for RecoverableParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RecoverableParseError::Eof =>
                f.write_str("The end of the file was reached.\n"),
            RecoverableParseError::WrongStartToken { rule, token, loc } =>
                write!(f, "At {}: Rule {} may not start with {}\n", loc, rule, token),
            RecoverableParseError::NoAlt { rule, alts } => {
                write!(f, "There is no valid alternative for {}:\n", rule)?;
                for (i, alt) in alts.iter().enumerate() {
                    write!(f, "{} alternative {}: {}", rule, i+1, alt)?;
                }
                Ok(())
            },
            RecoverableParseError::Context { rule, inner } =>
                write!(f, "At the start of rule {}:\n{}", rule, inner),
        }
    }
}

impl Display for UnrecoverableParseFailure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnrecoverableParseFailure::Token(err) =>
                write!(f, "{}\n", err),
            UnrecoverableParseFailure::Context { rule, loc, inner } =>
                write!(f, "At {}: Rule {}:\n{}", loc, rule, inner),
            UnrecoverableParseFailure::NoBacktrack { rule, loc, inner } =>
                write!(f, "At {}: Rule {}:\n{}", loc, rule, inner),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SymbolBinary => f.write_str("BINARY"),
            Token::SymbolDecimal => f.write_str("DECIMAL"),
            Token::SymbolHexadecimal => f.write_str("HEXADECIMAL"),
            Token::SymbolNumeral => f.write_str("NUMERAL"),
            Token::SymbolString => f.write_str("STRING"),
            Token::As => f.write_str("as"),
            Token::Let => f.write_str("let"),
            Token::Exists => f.write_str("exists"),
            Token::Forall => f.write_str("forall"),
            Token::Match => f.write_str("match"),
            Token::Par => f.write_str("par"),
            Token::ParenOpen => f.write_str("("),
            Token::ParenClose => f.write_str(")"),
            Token::Underscore => f.write_str("_"),
            Token::ExclamationPoint => f.write_str("!"),
            Token::Symbol(symb) => {
                if symb.len() > 0 && !lexer::is_digit(symb.chars().next().unwrap()) && symb.chars().all(lexer::is_symbol_char) {
                    f.write_str(symb)
                } else {
                    todo!()
                }
            }
            Token::Keyword(kwd) => write!(f, ":{}", kwd),
            Token::Numeral(n) => todo!(),
            Token::Decimal(n) => todo!(),
            Token::Hexadecimal(data) => todo!(),
            Token::Binary(data) => todo!(),
            Token::StringLiteral(s) => todo!(),
        }
    }
}

impl Display for ScriptCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(command)")
    }
}