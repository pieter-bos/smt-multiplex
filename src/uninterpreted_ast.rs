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