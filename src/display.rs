use std::fmt::{Display, Formatter, Write};
use bitvec::field::BitField;
use crate::{lexer, Location, RecoverableParseError, Token, TokenReaderErr, UnrecoverableParseFailure};
use crate::uninterpreted_ast::{Attribute, AttributeValue, CheckSatResponse, EchoResponse, GeneralFailure, GetAssertionsResponse, Identifier, Index, MatchCase, Pattern, QualifiedIdentifier, Reserved, ScriptCommand, SExpr, Sort, SortedVar, SpecConst, Term, VarBinding};

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
            Token::Numeral(n) => write!(f, "{}", n),
            Token::Decimal(n) => write!(f, "{}", n),
            Token::Hexadecimal(data) => {
                assert_eq!(data.len() % 4, 0);
                f.write_str("#x")?;
                for chunk in data.chunks(4) {
                    let index = chunk.load::<usize>();
                    assert!(index < 16);
                    let c = "0123456789abcdef".chars().nth(index).unwrap();
                    f.write_char(c)?;
                }
                Ok(())
            },
            Token::Binary(data) => {
                f.write_str("#x")?;
                for bit in data.iter() {
                    if *bit {
                        f.write_char('1')?;
                    } else {
                        f.write_char('0')?;
                    }
                }
                Ok(())
            },
            Token::StringLiteral(s) => {
                f.write_str("\"")?;
                for c in s.chars() {
                    assert!(lexer::is_printable(c) || lexer::is_whitespace(c));
                    if c == '"' {
                        f.write_str("\"\"")?;
                    } else {
                        f.write_char(c)?;
                    }
                }
                f.write_str("\"")?;
                Ok(())
            },
        }
    }
}

impl Display for SpecConst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tok = match self.clone() {
            SpecConst::Numeral(n) => Token::Numeral(n),
            SpecConst::Decimal(n) => Token::Decimal(n),
            SpecConst::Hexadecimal(data) => Token::Hexadecimal(data),
            SpecConst::Binary(data) => Token::Binary(data),
            SpecConst::String(s) => Token::StringLiteral(s),
        };
        write!(f, "{}", tok)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Simple(name) => f.write_str(name),
            Identifier::Indexed(name, indices) => {
                f.write_str("(_ ")?;
                f.write_str(name)?;
                for index in indices {
                    match index {
                        Index::Numeric(n) => write!(f, " {}", n)?,
                        Index::Symbol(s) => write!(f, " {}", s)?,
                    }
                }
                f.write_str(")")?;
                Ok(())
            },
        }
    }
}

impl Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            QualifiedIdentifier::Simple(id) => write!(f, "{}", id),
            QualifiedIdentifier::Qualified(id, sort) => write!(f, "(as {} {})", id, sort),
        }
    }
}

impl Display for Reserved {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Reserved::Binary => "BINARY",
            Reserved::Decimal => "DECIMAL",
            Reserved::Hexadecimal => "HEXADECIMAL",
            Reserved::Numeral => "NUMERAL",
            Reserved::String => "STRING",
            Reserved::Underscore => "_",
            Reserved::ExclamationPoint => "!",
            Reserved::As => "as",
            Reserved::Let => "let",
            Reserved::Exists => "exists",
            Reserved::Forall => "forall",
            Reserved::Match => "match",
            Reserved::Par => "par",
        })
    }
}

impl Display for SExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::Const(c) => write!(f, "{}", c),
            SExpr::Symbol(s) => f.write_str(s),
            SExpr::Reserved(reserved) => write!(f, "{}", reserved),
            SExpr::Keyword(kwd) => write!(f, ":{}", kwd),
            SExpr::Expr(args) => {
                if args.is_empty() {
                    f.write_str("()")?;
                } else {
                    f.write_str("(")?;
                    write!(f, "{}", args[0])?;
                    for arg in args.iter().skip(1) {
                        write!(f, " {}", arg)?;
                    }
                    f.write_str(")")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Attribute::Key(k) => f.write_str(k),
            Attribute::Pair(k, v) => {
                f.write_str(k)?;
                match v {
                    AttributeValue::Const(c) => write!(f, "{}", c)?,
                    AttributeValue::Symbol(s) => f.write_str(s)?,
                    AttributeValue::Expr(e) => {
                        if e.is_empty() {
                            f.write_str("()")?;
                        } else {
                            f.write_str("(")?;
                            write!(f, "{}", e[0])?;
                            for arg in e.iter().skip(1) {
                                write!(f, " {}", arg)?;
                            }
                            f.write_str(")")?;
                        }
                    },
                }
                Ok(())
            },
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Const(c) => write!(f, "{}", c),
            Term::Name(n) => write!(f, "{}", n),
            Term::Apply { function, args } => {
                f.write_str("(")?;
                write!(f, "{}", function)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                f.write_str(")")?;
                Ok(())
            }
            Term::Let { bindings, body } => {
                f.write_str("(let (")?;
                for VarBinding { name, value } in bindings {
                    write!(f, "({} {})", name, value)?;
                }
                f.write_str(") ")?;
                write!(f, "{}", body)?;
                f.write_str(")")?;
                Ok(())
            },
            Term::Forall { bindings, body } => {
                f.write_str("(forall (")?;
                for SortedVar { name, sort } in bindings {
                    write!(f, "({} {})", name, sort)?;
                }
                f.write_str(") ")?;
                write!(f, "{}", body)?;
                f.write_str(")")?;
                Ok(())
            },
            Term::Exists { bindings, body } => {
                f.write_str("(exists (")?;
                for SortedVar { name, sort } in bindings {
                    write!(f, "({} {})", name, sort)?;
                }
                f.write_str(") ")?;
                write!(f, "{}", body)?;
                f.write_str(")")?;
                Ok(())
            },
            Term::Match { term, cases } => {
                f.write_str("(match ")?;
                write!(f, "{}", term)?;
                f.write_str(" (")?;
                for (i, MatchCase { pattern, result }) in cases.iter().enumerate() {
                    if i == 0 {
                        f.write_str("(")?;
                    } else {
                        f.write_str(" (")?;
                    }
                    match pattern {
                        Pattern::Binding(name) => f.write_str(name)?,
                        Pattern::Application { function, args } => {
                            f.write_str("(")?;
                            f.write_str(function)?;
                            for arg in args {
                                f.write_char(' ')?;
                                f.write_str(arg)?;
                            }
                            f.write_str(")")?;
                        }
                    }
                    f.write_str(" ")?;
                    write!(f, "{}", result)?;
                    f.write_str(")")?;
                }
                f.write_str(")")?;
                Ok(())
            },
            Term::Attributed { term, attributes } => {
                f.write_str("(! ")?;
                write!(f, "{}", term)?;
                for attribute in attributes {
                    write!(f, " {}", attribute)?;
                }
                f.write_str(")")?;
                Ok(())
            },
        }
    }
}

impl Display for Sort {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Sort::Name(id) => write!(f, "{}", id),
            Sort::Parametric(name, args) => {
                f.write_str("(")?;
                write!(f, "{}", name)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                f.write_str(")")?;
                Ok(())
            },
        }
    }
}

impl Display for ScriptCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScriptCommand::Assert(term) =>
                write!(f, "(assert {})", term),
            ScriptCommand::CheckSat =>
                f.write_str("(check-sat)"),
            ScriptCommand::CheckSatAssuming(_) => todo!(),
            ScriptCommand::DeclareConst(_, _) => todo!(),
            ScriptCommand::DeclareDatatype(_, _) => todo!(),
            ScriptCommand::DeclareDatatypes(_, _) => todo!(),
            ScriptCommand::DeclareFun { .. } => todo!(),
            ScriptCommand::DeclareSort(_, _) => todo!(),
            ScriptCommand::DefineConst(_, _, _) => todo!(),
            ScriptCommand::DefineFun(_) => todo!(),
            ScriptCommand::DefineFunRec(_) => todo!(),
            ScriptCommand::DefineFunsRec(_, _) => todo!(),
            ScriptCommand::DefineSort { .. } => todo!(),
            ScriptCommand::Echo(_) => todo!(),
            ScriptCommand::Exit => todo!(),
            ScriptCommand::GetAssertions => todo!(),
            ScriptCommand::GetAssignment => todo!(),
            ScriptCommand::GetInfo(_) => todo!(),
            ScriptCommand::GetModel => todo!(),
            ScriptCommand::GetOption(_) => todo!(),
            ScriptCommand::GetProof => todo!(),
            ScriptCommand::GetUnsatAssumptions => todo!(),
            ScriptCommand::GetUnsatCore => todo!(),
            ScriptCommand::GetValue(_) => todo!(),
            ScriptCommand::Pop(_) => todo!(),
            ScriptCommand::Push(_) => todo!(),
            ScriptCommand::Reset => todo!(),
            ScriptCommand::ResetAssertions => todo!(),
            ScriptCommand::SetInfo(_) => todo!(),
            ScriptCommand::SetLogic(_) => todo!(),
            ScriptCommand::SetOption(_) => todo!(),
        }
    }
}

impl Display for GeneralFailure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneralFailure::Unsupported => f.write_str("unsupported"),
            GeneralFailure::Error(message) => {
                f.write_str("(error ")?;
                write!(f, "{}", Token::StringLiteral(message.clone()))?;
                f.write_str(")")?;
                Ok(())
            },
        }
    }
}

impl Display for CheckSatResponse {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CheckSatResponse::Sat => f.write_str("sat"),
            CheckSatResponse::Unsat => f.write_str("unsat"),
            CheckSatResponse::Unknown => f.write_str("unknown"),
        }
    }
}

impl Display for EchoResponse {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Token::StringLiteral(self.0.clone()))
    }
}

impl Display for GetAssertionsResponse {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}