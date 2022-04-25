use bigdecimal::BigDecimal;
use bit_vec::BitVec;
use num_bigint::BigUint;

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SpecConst {
    Numeral(BigUint),
    Decimal(BigDecimal),
    Hexadecimal(BitVec),
    Binary(BitVec),
    String(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SExpr {
    Const(SpecConst),
    Symbol(String),
    Reserved(Reserved),
    Keyword(String),
    Expr(Vec<SExpr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Index {
    Numeric(BigUint),
    Symbol(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Identifier {
    Simple(String),
    Indexed(String, Vec<Index>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AttributeValue {
    Const(SpecConst),
    Symbol(String),
    Expr(Vec<SExpr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Attribute {
    Key(String),
    Pair(String, AttributeValue),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Sort {
    Name(Identifier),
    Parametric(Identifier, Vec<Sort>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum QualifiedIdentifier {
    Simple(Identifier),
    Qualified(Identifier, Sort),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarBinding {
    pub name: String,
    pub value: Term,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SortedVar {
    pub name: String,
    pub sort: Sort,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pattern {
    Binding(String),
    Application { function: String, args: Vec<String> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub result: Term,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SortDec {
    pub name: String,
    pub idx: BigUint,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SelectorDec {
    pub name: String,
    pub sort: Sort,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstructorDec {
    pub name: String,
    pub selectors: Vec<SelectorDec>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DatatypeDec {
    pub params: Vec<String>,
    pub constructors: Vec<ConstructorDec>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionDec {
    pub name: String,
    pub sort: Sort,
    pub args: Vec<SortedVar>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionDef {
    pub dec: FunctionDec,
    pub body: Term,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PropLiteral {
    Positive(String),
    Negative(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ScriptCommand {
    Assert(Term),
    CheckSat,
    CheckSatAssuming(Vec<PropLiteral>),
    DeclareConst(String, Sort),
    DeclareDatatype(String, DatatypeDec),
    DeclareDatatypes(Vec<SortDec>, Vec<DatatypeDec>),
    DeclareFun { name: String, sort: Sort, args: Vec<Sort> },
    DeclareSort(String, BigUint),
    DefineConst(String, Sort, Term),
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