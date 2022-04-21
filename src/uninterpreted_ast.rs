use bigdecimal::BigDecimal;
use bit_vec::BitVec;
use num_bigint::BigUint;

pub enum Reserved {
    Binary,
    Decimal,
    Hexadecimal,
    Numeral,
    String,
    Underscore,
    ExclamationPoint,
    As,
    Let,
    Exists,
    Forall,
    Match,
    Par,
}

pub enum SpecConst {
    Numeral(BigUint),
    Decimal(BigDecimal),
    Hexadecimal(BitVec),
    Binary(BitVec),
    String(String),
}

pub enum SExpr {
    Const(SpecConst),
    Symbol(String),
    Reserved(Reserved),
    Keyword(String),
    Expr(Vec<SExpr>),
}

pub enum Index {
    Numeric(BigUint),
    Symbol(String),
}

pub enum Identifier {
    Simple(String),
    Indexed(String, Vec<Index>),
}

pub enum AttributeValue {
    Const(SpecConst),
    Symbol(String),
    Expr(Vec<SExpr>),
}

pub enum Attribute {
    Key(String),
    Pair(String, AttributeValue),
}

pub enum Sort {
    Name(Identifier),
    Parametric(Identifier, Vec<Sort>),
}

pub enum QualifiedIdentifier {
    Simple(Identifier),
    Qualified(Identifier, Sort),
}

pub struct VarBinding {
    pub name: String,
    pub value: Term,
}

pub struct SortedVar {
    pub name: String,
    pub sort: Sort,
}

pub enum Pattern {
    Binding(String),
    Application { function: String, args: Vec<String> },
}

pub struct MatchCase {
    pub pattern: Pattern,
    pub result: Term,
}

pub enum Term {
    Const(SpecConst),
    Name(QualifiedIdentifier),
    Apply { function: QualifiedIdentifier, args: Vec<Term> },
    Let { bindings: Vec<VarBinding>, body: Box<Term> },
    Forall { bindings: Vec<SortedVar>, body: Box<Term> },
    Exists { bindings: Vec<SortedVar>, body: Box<Term> },
    Match { term: Box<Term>, cases: Vec<MatchCase> },
    Attributed { term: Box<Term>, attributes: Vec<Attribute> },
}

pub struct SortDec {
    pub name: String,
    pub idx: BigUint,
}

pub struct SelectorDec {
    pub name: String,
    pub sort: Sort,
}

pub struct ConstructorDec {
    pub name: String,
    pub selectors: Vec<SelectorDec>,
}

pub struct DatatypeDec {
    pub params: Vec<String>,
    pub constructors: Vec<ConstructorDec>,
}

pub struct FunctionDec {
    pub name: String,
    pub sort: Sort,
    pub args: Vec<SortedVar>,
}

pub struct FunctionDef {
    pub dec: FunctionDec,
    pub body: Term,
}

pub enum PropLiteral {
    Positive(String),
    Negative(String),
}

pub enum ScriptCommand {
    Assert(Term),
    CheckSat,
    CheckSatAssuming(Vec<PropLiteral>),
    DeclareConst(String, Sort),
    DeclareDatatype(String, DatatypeDec),
    DeclareDatatypes(Vec<SortDec>, Vec<DatatypeDec>),
    DeclareFun(FunctionDec),
    DeclareSort(String, BigUint),
    DefineFun(FunctionDef),
    DefineFunRec(FunctionDef),
    DefineFunsRec(Vec<FunctionDec>, Vec<Term>),
    DefineSort { name: String, args: Vec<String>, def: Sort },
    Echo(String),
    Exit,
    GetAssertions,
    GetAssignment,
    GetInfo(String),
    GetModel,
    GetOption(String),
    GetProof,
    GetUnsatAssumptions,
    GetUnsatCore,
    GetValue(Vec<Term>),
    Pop(BigUint),
    Push(BigUint),
    Reset,
    ResetAssertions,
    SetInfo(Attribute),
    SetLogic(String),
    SetOption(Attribute),
}