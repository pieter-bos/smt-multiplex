use std::cell::RefCell;
use std::rc::Rc;
use num_bigint::BigUint;

struct Name {
    name: String,

}

struct SortDec {
    name: Name,
    arity: BigUint,
}

type Sort = Rc<RefCell<SortDec>>;

struct FuncDec {
    name: Name,
    result: Sort,
    args: Vec<Sort>,
}



struct State {

}