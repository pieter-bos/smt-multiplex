use num_bigint::BigUint;

pub enum Index {
    Numeric(BigUint),
    Symbol(String),
}

pub enum Identifier {
    Simple(String),
    Indexed(String, Vec<Index>),
}