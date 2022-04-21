use std::io::Read;
use std::process::Command;
use std::result;
use bigdecimal::BigDecimal;
use bit_vec::BitVec;
use num_bigint::{BigInt, BigUint};
use crate::{lexer, Token, TokenReaderErr};
use crate::parser::PeekResult::*;
use crate::parser::RecoverableParseError::WrongStartToken;
use crate::uninterpreted_ast::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Location {
    line_idx: u64,
    col_idx: u64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Context {
    rule: &'static str,
    loc: Location,
}

macro_rules! expect {
    ($ctx: expr, $res: expr) => {
        match $res {
            Err(e) => { return Err(e.context($ctx)); },
            Ok(Err(e)) => { return Ok(Err(RecoverableParseError::Context { rule: $ctx.rule, inner: Box::new(e) })) },
            Ok(Ok(e)) => e
        }
    }
}

macro_rules! then_expect {
    ($ctx: expr, $res:expr) => {
        match $res {
            Err(e) => { return Err(e.context($ctx)); },
            Ok(Err(e)) => { return Err(UnrecoverableParseFailure::NoBacktrack { rule: $ctx.rule, loc: $ctx.loc, inner: e }); },
            Ok(Ok(e)) => e
        }
    }
}

enum RecoverableParseError {
    Eof,
    WrongStartToken { rule: &'static str, token: lexer::Token, loc: Location },
    NoAlt { rule: &'static str, alts: Vec<RecoverableParseError> },
    Context { rule: &'static str, inner: Box<RecoverableParseError> },
}

enum UnrecoverableParseFailure {
    Token(lexer::TokenReaderErr),
    Unexpected { rule: &'static str, token: lexer::Token, expected: &'static str, loc: Location },
    Context { rule: &'static str, loc: Location, inner: Box<UnrecoverableParseFailure> },
    NoBacktrack { rule: &'static str, loc: Location, inner: RecoverableParseError },
}

impl UnrecoverableParseFailure {
    fn context(self, ctx: Context) -> Self {
        UnrecoverableParseFailure::Context {
            rule: ctx.rule, loc: ctx.loc, inner: Box::new(self)
        }
    }
}

type Result<T> = result::Result<result::Result<T, RecoverableParseError>, UnrecoverableParseFailure>;

enum PeekResult {
    Unread(Token),
    Consumed,
}

struct Parser<R: Read> {
    lexer: lexer::TokenReader<R>,
    buf: Option<lexer::Token>,
}

impl <R: Read> Parser<R> {
    fn new(lexer: lexer::TokenReader<R>) -> Self {
        Self {
            lexer, buf: None,
        }
    }

    fn peek<T, F: FnOnce(lexer::Token) -> (Result<T>, PeekResult)>(&mut self, f: F) -> Result<T> {
        let (res, tok) = match self.buf.take() {
            Some(tok) => f(tok),
            None => f(match self.lexer.next() {
                None => match self.lexer.consume_err() {
                    None => return Ok(Err(RecoverableParseError::Eof)),
                    Some(e) => return Err(UnrecoverableParseFailure::Token(e)),
                },
                Some(tok) => tok
            }),
        };
        self.buf = match tok {
            Unread(tok) => Some(tok),
            Consumed => None,
        };
        res
    }

    fn get_loc(&self) -> Location {
        Location {
            line_idx: self.lexer.get_line_idx(),
            col_idx: self.lexer.get_col_idx(),
        }
    }

    fn ctx(&self, rule: &'static str) -> Context {
        Context { rule, loc: self.get_loc() }
    }

    fn alts<T>(&mut self, ctx: Context, alts: Vec<&dyn Fn(&mut Self) -> Result<T>>) -> Result<T> {
        let mut alt_errs = Vec::<RecoverableParseError>::new();

        for f in alts {
            match f(self).map_err(|e| e.context(ctx))? {
                Ok(t) => { return Ok(Ok(t)); }
                Err(rec_err) => { alt_errs.push(rec_err); }
            }
        }

        Ok(Err(RecoverableParseError::NoAlt { rule: ctx.rule, alts: alt_errs }))
    }

    fn star<T>(&mut self, what: &dyn Fn(&mut Self) -> Result<T>) -> Result<Vec<T>> {
        let mut buf = Vec::<T>::new();

        loop {
            match what(self) {
                Err(unrec_err) => { return Err(unrec_err); },
                Ok(Err(_)) => /* recover */ { return Ok(Ok(buf)); },
                Ok(Ok(t)) => { buf.push(t); },
            }
        }
    }

    fn plus<T>(&mut self, what: &dyn Fn(&mut Self) -> Result<T>) -> Result<Vec<T>> {
        let mut buf = Vec::<T>::new();

        buf.push(expect!(self.ctx("<first value>"), what(self)));

        loop {
            match what(self) {
                Err(unrec_err) => { return Err(unrec_err); },
                Ok(Err(_)) => /* recover */ { return Ok(Ok(buf)); },
                Ok(Ok(t)) => { buf.push(t); },
            }
        }
    }

    fn maybe<T>(&mut self, what: &dyn Fn(&mut Self) -> Result<T>) -> Result<Option<T>> {
        match what(self) {
            Err(unrec_err) => Err(unrec_err),
            Ok(Err(_)) => /* recover */ Ok(Ok(None)),
            Ok(Ok(t)) => Ok(Ok(Some(t))),
        }
    }

    pub fn numeral(&mut self) -> Result<BigUint> {
        let ctx = self.ctx("numeral");
        self.peek(|tok| match tok {
            Token::Numeral(n) => (Ok(Ok(n)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok)),
        })
    }

    pub fn decimal(&mut self) -> Result<BigDecimal> {
        let ctx = self.ctx("decimal");
        self.peek(|tok| match tok {
            Token::Decimal(n) => (Ok(Ok(n)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok)),
        })
    }

    pub fn hexadecimal(&mut self) -> Result<BitVec> {
        let ctx = self.ctx("hexadecimal");
        self.peek(|tok| match tok {
            Token::Hexadecimal(n) => (Ok(Ok(n)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok)),
        })
    }

    pub fn binary(&mut self) -> Result<BitVec> {
        let ctx = self.ctx("binary");
        self.peek(|tok| match tok {
            Token::Binary(n) => (Ok(Ok(n)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok)),
        })
    }

    pub fn string(&mut self) -> Result<String> {
        let ctx = self.ctx("string");
        self.peek(|tok| match tok {
            Token::StringLiteral(s) => (Ok(Ok(s)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok)),
        })
    }

    pub fn symbol(&mut self) -> Result<String> {
        let ctx = self.ctx("symbol");
        self.peek(|tok| match tok {
            Token::Symbol(s) => (Ok(Ok(s)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok))
        })
    }

    pub fn keyword(&mut self) -> Result<String> {
        let ctx = self.ctx("keyword");
        self.peek(|tok| match tok {
            Token::Keyword(s) => (Ok(Ok(s)), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok))
        })
    }

    pub fn paren_open(&mut self) -> Result<()> {
        let ctx = self.ctx("paren_open");
        self.peek(|tok| match tok {
            Token::ParenOpen => (Ok(Ok(())), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok))
        })
    }

    pub fn paren_close(&mut self) -> Result<()> {
        let ctx = self.ctx("paren_close");
        self.peek(|tok| match tok {
            Token::ParenClose => (Ok(Ok(())), Consumed),
            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok))
        })
    }

    pub fn exactly(&mut self, expect: Token) -> Result<()> {
        let ctx = self.ctx("<token>");
        self.peek(|tok| if(tok == expect) {
            (Ok(Ok(())), Consumed)
        } else {
            (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, token: tok.clone(), loc: ctx.loc })), Unread(tok))
        })
    }

    pub fn reserved(&mut self) -> Result<Reserved> {
        let ctx = self.ctx("reserved");
        self.peek(|tok| match tok {
            Token::SymbolBinary => (Ok(Ok(Reserved::Binary)), Consumed),
            Token::SymbolDecimal => (Ok(Ok(Reserved::Decimal)), Consumed),
            Token::SymbolHexadecimal => (Ok(Ok(Reserved::Hexadecimal)), Consumed),
            Token::SymbolNumeral => (Ok(Ok(Reserved::Numeral)), Consumed),
            Token::SymbolString => (Ok(Ok(Reserved::String)), Consumed),

            Token::As => (Ok(Ok(Reserved::As)), Consumed),
            Token::Let => (Ok(Ok(Reserved::Let)), Consumed),
            Token::Exists => (Ok(Ok(Reserved::Exists)), Consumed),
            Token::Forall => (Ok(Ok(Reserved::Forall)), Consumed),
            Token::Match => (Ok(Ok(Reserved::Match)), Consumed),
            Token::Par => (Ok(Ok(Reserved::Par)), Consumed),

            Token::Underscore => (Ok(Ok(Reserved::Underscore)), Consumed),
            Token::ExclamationPoint => (Ok(Ok(Reserved::ExclamationPoint)), Consumed),

            _ => (Ok(Err(RecoverableParseError::WrongStartToken { rule: ctx.rule, loc: ctx.loc, token: tok.clone() })), Unread(tok))
        })
    }

    pub fn spec_constant(&mut self) -> Result<SpecConst> {
        let ctx = self.ctx("spec_constant");
        self.alts(ctx, vec![
            &|this: &mut Self| { let n = expect!(ctx, this.numeral()); Ok(Ok(SpecConst::Numeral(n))) },
            &|this: &mut Self| { let n = expect!(ctx, this.decimal()); Ok(Ok(SpecConst::Decimal(n))) },
            &|this: &mut Self| { let n = expect!(ctx, this.hexadecimal()); Ok(Ok(SpecConst::Hexadecimal(n))) },
            &|this: &mut Self| { let n = expect!(ctx, this.binary()); Ok(Ok(SpecConst::Binary(n))) },
            &|this: &mut Self| { let n = expect!(ctx, this.string()); Ok(Ok(SpecConst::String(n))) },
        ])
    }

    pub fn s_expr_expr(&mut self) -> Result<Vec<SExpr>> {
        let ctx = self.ctx("s_expr_expr");
        expect!(ctx, self.paren_open());
        let args = then_expect!(ctx, self.star(&|this: &mut Self| this.s_expr()));
        then_expect!(ctx, self.paren_close());
        Ok(Ok(args))
    }

    pub fn s_expr(&mut self) -> Result<SExpr> {
        let ctx = self.ctx("s_expr");
        self.alts(ctx, vec![
            &|this: &mut Self| { let c = expect!(ctx, this.spec_constant()); Ok(Ok(SExpr::Const(c))) },
            &|this: &mut Self| { let symb = expect!(ctx, this.symbol()); Ok(Ok(SExpr::Symbol(symb))) },
            &|this: &mut Self| { let res = expect!(ctx, this.reserved()); Ok(Ok(SExpr::Reserved(res))) },
            &|this: &mut Self| { let kwd = expect!(ctx, this.keyword()); Ok(Ok(SExpr::Keyword(kwd))) },
            &|this: &mut Self| { let args = expect!(ctx, this.s_expr_expr()); Ok(Ok(SExpr::Expr(args))) },
        ])
    }

    pub fn index(&mut self) -> Result<Index> {
        let ctx = self.ctx("index");
        self.alts(ctx, vec![
            &|this: &mut Self| { let n = expect!(ctx, this.numeral()); Ok(Ok(Index::Numeric(n))) },
            &|this: &mut Self| { let s = expect!(ctx, this.symbol()); Ok(Ok(Index::Symbol(s))) },
        ])
    }

    pub fn indexed_identifier_completion(&mut self) -> Result<Identifier> {
        let ctx = self.ctx("indexed_identifier_completion");
        let name = then_expect!(ctx, self.symbol());
        let indices = then_expect!(ctx, self.plus(&|this: &mut Self| this.index()));
        then_expect!(ctx, self.paren_close());
        Ok(Ok(Identifier::Indexed(name, indices)))
    }

    pub fn identifier(&mut self) -> Result<Identifier> {
        let ctx = self.ctx("identifier");
        self.alts(ctx, vec![
            &|this: &mut Self| { let name = expect!(ctx, this.symbol()); Ok(Ok(Identifier::Simple(name))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                then_expect!(ctx, this.exactly(Token::ExclamationPoint));
                this.indexed_identifier_completion()
            },
        ])
    }

    pub fn attribute_value(&mut self) -> Result<AttributeValue> {
        let ctx = self.ctx("attribute_value");
        self.alts(ctx, vec![
            &|this: &mut Self| { let c = expect!(ctx, this.spec_constant()); Ok(Ok(AttributeValue::Const(c))) },
            &|this: &mut Self| { let symb = expect!(ctx, this.symbol()); Ok(Ok(AttributeValue::Symbol(symb))) },
            &|this: &mut Self| { let args = expect!(ctx, this.s_expr_expr()); Ok(Ok(AttributeValue::Expr(args))) },
        ])
    }

    pub fn attribute(&mut self) -> Result<Attribute> {
        let ctx = self.ctx("attribute");
        let key = expect!(ctx, self.keyword());
        let value = then_expect!(ctx, self.maybe(&|this: &mut Self| this.attribute_value()));

        Ok(Ok(match value {
            None => Attribute::Key(key),
            Some(val) => Attribute::Pair(key, val)
        }))
    }

    pub fn sort(&mut self) -> Result<Sort> {
        let ctx = self.ctx("sort");

        self.alts(ctx, vec![
            &|this: &mut Self| { let name = expect!(ctx, this.identifier()); Ok(Ok(Sort::Name(name))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                let name = then_expect!(ctx, this.identifier());
                let args = then_expect!(ctx, this.plus(&|this: &mut Self| this.sort()));
                then_expect!(ctx, this.paren_close());
                Ok(Ok(Sort::Parametric(name, args)))
            }
        ])
    }

    pub fn qual_identifier_completion(&mut self) -> Result<QualifiedIdentifier> {
        let ctx = self.ctx("qual_identifier_completion");
        let name = then_expect!(ctx, self.identifier());
        let sort = then_expect!(ctx, self.sort());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(QualifiedIdentifier::Qualified(name, sort)))
    }

    pub fn qual_identifier(&mut self) -> Result<QualifiedIdentifier> {
        let ctx = self.ctx("qual_identifier");
        self.alts(ctx, vec![
            &|this: &mut Self| { let id = expect!(ctx, this.identifier()); Ok(Ok(QualifiedIdentifier::Simple(id))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                then_expect!(ctx, this.exactly(Token::As));
                this.qual_identifier_completion()
            },
        ])
    }

    pub fn var_binding(&mut self) -> Result<VarBinding> {
        let ctx = self.ctx("var_binding");
        expect!(ctx, self.paren_open());
        let name = then_expect!(ctx, self.symbol());
        let value = then_expect!(ctx, self.term());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(VarBinding { name, value }))
    }

    pub fn sorted_var(&mut self) -> Result<SortedVar> {
        let ctx = self.ctx("sorted_var");
        expect!(ctx, self.paren_open());
        let name = then_expect!(ctx, self.symbol());
        let sort = then_expect!(ctx, self.sort());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(SortedVar { name, sort }))
    }

    pub fn pattern(&mut self) -> Result<Pattern> {
        let ctx = self.ctx("pattern");
        self.alts(ctx, vec![
            &|this: &mut Self| { let name = expect!(ctx, this.symbol()); Ok(Ok(Pattern::Binding(name))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                let function = then_expect!(ctx, this.symbol());
                let args = then_expect!(ctx, this.plus(&|this: &mut Self| this.symbol()));
                then_expect!(ctx, this.paren_close());
                Ok(Ok(Pattern::Application { function, args }))
            },
        ])
    }

    pub fn match_case(&mut self) -> Result<MatchCase> {
        let ctx = self.ctx("match_case");
        expect!(ctx, self.paren_open());
        let pattern = then_expect!(ctx, self.pattern());
        let result = then_expect!(ctx, self.term());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(MatchCase { pattern, result }))
    }

    pub fn term(&mut self) -> Result<Term> {
        let ctx = self.ctx("term");
        self.alts(ctx, vec![
            &|this: &mut Self| { let c = expect!(ctx, this.spec_constant()); Ok(Ok(Term::Const(c))) },
            &|this: &mut Self| { let name = expect!(ctx, this.symbol()); Ok(Ok(Term::Name(QualifiedIdentifier::Simple(Identifier::Simple(name))))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                let result = this.alts(ctx, vec![
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::Underscore));
                        let id = then_expect!(ctx, this.indexed_identifier_completion());
                        Ok(Ok(Term::Name(QualifiedIdentifier::Simple(id))))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::As));
                        let id = then_expect!(ctx, this.qual_identifier_completion());
                        Ok(Ok(Term::Name(id)))
                    },
                    &|this: &mut Self| {
                        let function = expect!(ctx, this.qual_identifier());
                        let args = then_expect!(ctx, this.plus(&|this: &mut Self| this.term()));
                        Ok(Ok(Term::Apply { function, args }))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::Let));
                        then_expect!(ctx, this.paren_open());
                        let bindings = then_expect!(ctx, this.plus(&|this: &mut Self| this.var_binding()));
                        then_expect!(ctx, this.paren_close());
                        let body = then_expect!(ctx, this.term());
                        Ok(Ok(Term::Let { bindings, body: Box::new(body) }))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::Forall));
                        then_expect!(ctx, this.paren_open());
                        let bindings = then_expect!(ctx, this.plus(&|this: &mut Self| this.sorted_var()));
                        then_expect!(ctx, this.paren_close());
                        let body = then_expect!(ctx, this.term());
                        Ok(Ok(Term::Forall { bindings, body: Box::new(body) }))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::Exists));
                        then_expect!(ctx, this.paren_open());
                        let bindings = then_expect!(ctx, this.plus(&|this: &mut Self| this.sorted_var()));
                        then_expect!(ctx, this.paren_close());
                        let body = then_expect!(ctx, this.term());
                        Ok(Ok(Term::Exists { bindings, body: Box::new(body) }))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::Match));
                        let term = then_expect!(ctx, this.term());
                        then_expect!(ctx, this.paren_open());
                        let cases = then_expect!(ctx, this.plus(&|this: &mut Self| this.match_case()));
                        then_expect!(ctx, this.paren_close());
                        Ok(Ok(Term::Match { term: Box::new(term), cases }))
                    },
                    &|this: &mut Self| {
                        expect!(ctx, this.exactly(Token::ExclamationPoint));
                        let term = then_expect!(ctx, this.term());
                        let attributes = then_expect!(ctx, this.plus(&|this: &mut Self| this.attribute()));
                        Ok(Ok(Term::Attributed { term: Box::new(term), attributes }))
                    },
                ]);
                then_expect!(ctx, this.paren_close());
                result
            },
        ])
    }

    pub fn sort_dec(&mut self) -> Result<SortDec> {
        let ctx = self.ctx("sort_dec");
        expect!(ctx, self.paren_open());
        let name = then_expect!(ctx, self.symbol());
        let idx = then_expect!(ctx, self.numeral());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(SortDec { name, idx }))
    }

    pub fn selector_dec(&mut self) -> Result<SelectorDec> {
        let ctx = self.ctx("selector_dec");
        expect!(ctx, self.paren_open());
        let name = then_expect!(ctx, self.symbol());
        let sort = then_expect!(ctx, self.sort());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(SelectorDec { name, sort }))
    }

    pub fn constructor_dec(&mut self) -> Result<ConstructorDec> {
        let ctx = self.ctx("constructor_dec");
        expect!(ctx, self.paren_open());
        let name = then_expect!(ctx, self.symbol());
        let selectors = then_expect!(ctx, self.star(&|this: &mut Self| this.selector_dec()));
        then_expect!(ctx, self.paren_close());
        Ok(Ok(ConstructorDec { name, selectors }))
    }

    pub fn datatype_dec(&mut self) -> Result<DatatypeDec> {
        let ctx = self.ctx("datatype_dec");
        expect!(ctx, self.paren_open());
        let result = then_expect!(ctx, self.alts(ctx, vec![
            &|this: &mut Self| {
                let constructors = expect!(ctx, this.plus(&|this: &mut Self| this.constructor_dec()));
                Ok(Ok(DatatypeDec { params: vec![], constructors }))
            },
            &|this: &mut Self| {
                expect!(ctx, this.exactly(Token::Par));
                then_expect!(ctx, this.paren_open());
                let params = then_expect!(ctx, this.plus(&|this: &mut Self| this.symbol()));
                then_expect!(ctx, this.paren_close());
                then_expect!(ctx, this.paren_open());
                let constructors = then_expect!(ctx, this.plus(&|this: &mut Self| this.constructor_dec()));
                then_expect!(ctx, this.paren_close());
                Ok(Ok(DatatypeDec { params, constructors }))
            },
        ]));
        then_expect!(ctx, self.paren_close());
        Ok(Ok(result))
    }

    pub fn function_dec_inner(&mut self) -> Result<FunctionDec> {
        let ctx = self.ctx("function_dec_inner");
        let name = expect!(ctx, self.symbol());
        then_expect!(ctx, self.paren_open());
        let args = then_expect!(ctx, self.star(&|this: &mut Self| this.sorted_var()));
        then_expect!(ctx, self.paren_close());
        let sort = then_expect!(ctx, self.sort());
        Ok(Ok(FunctionDec { name, sort, args }))
    }

    pub fn function_dec(&mut self) -> Result<FunctionDec> {
        let ctx = self.ctx("function_dec");
        expect!(ctx, self.paren_open());
        let result = then_expect!(ctx, self.function_dec_inner());
        then_expect!(ctx, self.paren_close());
        Ok(Ok(result))
    }

    pub fn function_def(&mut self) -> Result<FunctionDef> {
        let ctx = self.ctx("function_def");
        let dec = expect!(ctx, self.function_dec_inner());
        let body = then_expect!(ctx, self.term());
        Ok(Ok(FunctionDef { dec, body }))
    }

    pub fn prop_literal(&mut self) -> Result<PropLiteral> {
        let ctx = self.ctx("prop_literal");
        self.alts(ctx, vec![
            &|this: &mut Self| { let name = expect!(ctx, this.symbol()); Ok(Ok(PropLiteral::Positive(name))) },
            &|this: &mut Self| {
                expect!(ctx, this.paren_open());
                then_expect!(ctx, this.exactly(Token::Symbol("not".into())));
                let name = then_expect!(ctx, this.symbol());
                then_expect!(ctx, this.paren_close());
                Ok(Ok(PropLiteral::Negative(name)))
            },
        ])
    }

    pub fn command(&mut self) -> Result<ScriptCommand> {
        let ctx = self.ctx("command");
        expect!(ctx, self.paren_open());
        let result = then_expect!(ctx, self.alts(ctx, vec![
            &|this: &mut Self| {
                expect!(ctx, this.exactly(Token::Symbol("assert".into())));
                Ok(Ok(ScriptCommand::Assert(then_expect!(ctx, this.term()))))
            },
            &|this: &mut Self| {
                expect!(ctx, this.exactly(Token::Symbol("check-sat".into())));
                Ok(Ok(ScriptCommand::CheckSat))
            },
        ]));
        then_expect!(ctx, self.paren_close());
        Ok(Ok(result))
    }
}