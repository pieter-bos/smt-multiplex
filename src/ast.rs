use std::cell::RefCell;
use std::rc::Rc;
use bigdecimal::BigDecimal;
use num_bigint::BigUint;
use crate::uninterpreted_ast;

struct AbstractSortDec {
    name: String,
    arity: BigUint,
}

type AbstractSort = Rc<RefCell<AbstractSortDec>>;

struct SortBindingDec {
    name: String,
}

type SortBinding = Rc<RefCell<SortBindingDec>>;

struct DatatypeDec {
    name: String,
    args: Vec<SortBindingDec>,
    cons: Vec<DatatypeConstructorDec>,
}

struct DatatypeConstructorDec {
    name: String,
    fields: Vec<DatatypeFieldDec>,
}

struct DatatypeFieldDec {
    name: String,
    sort: Sort,
}

type DatatypeField = Rc<RefCell<DatatypeFieldDec>>;
type DatatypeConstructor = Rc<RefCell<DatatypeConstructorDec>>;
type Datatype = Rc<RefCell<DatatypeDec>>;

struct AliasSortDec {
    name: String,
    args: Vec<SortBindingDec>,
    def: Sort,
}

type AliasSort = Rc<RefCell<AliasSortDec>>;

enum Sort {
    Abstract(AbstractSort),
    Datatype(Datatype),
    Alias(AliasSort),
    Binding(SortBinding),
}

// Functions and constants
struct FuncDec {
    name: String,
    result: Sort,
    args: Vec<Sort>,
}

type Func = Rc<RefCell<FuncDec>>;

// Let bindings
struct BindingDec {
    name: String,
    value: Term,
}

type Binding = Rc<RefCell<BindingDec>>;

// Quantifier bindings
struct VariableDec {
    name: String,
    sort: Sort,
}

type Variable = Rc<RefCell<VariableDec>>;

struct MatchBindingDec {
    name: String,
}

type MatchBinding = Rc<RefCell<MatchBindingDec>>;

enum Name {
    Func(Func),
    Binding(Binding),
    Variable(Variable),
    MatchBinding(Variable),
    Constructor(DatatypeConstructor),
    Destructor(DatatypeField),
}

enum Identifier {
    Simple(Name),
    Typed(Name, Sort),
}

enum Pattern {
    Default(MatchBindingDec),
    Destructor(DatatypeConstructor, Vec<MatchBindingDec>),
}

enum Term {
    LiteralInt(BigUint),
    LiteralReal(BigDecimal),
    LiteralString(String),
    Apply(Identifier, Vec<Term>),
    Let(Vec<BindingDec>, Box<Term>),
    Forall(Vec<VariableDec>, Box<Term>),
    Exists(Vec<VariableDec>, Box<Term>),
    Match(Box<Term>, Vec<(Pattern, Term)>),
    Attributed(Box<Term>, Vec<uninterpreted_ast::Attribute>),
}

enum Declaration {
    AbstractSort(AbstractSortDec),
    Datatype(DatatypeDec),
    AliasSort(AliasSortDec),
    Func(FuncDec),
}

struct Scope {
    decs: Vec<Declaration>,
    assertions: Vec<Term>,
}

struct State {
    global_declarations: bool,
    global_definitions: bool,
    global_scope: Vec<Declaration>,
    stack: Vec<Declaration>,
}