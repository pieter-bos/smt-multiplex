mod lexer;
mod parser;
mod uninterpreted_ast;

use std::fs::File;
use std::io::BufReader;
use lexer::*;

fn main() {
    let f = File::open("/home/pieter/vercors/tmp/logfile-00.smt2").unwrap();
    let mut reader = TokenReader::new(BufReader::new(f));

    for tok in &mut reader {
        println!("{:?}", tok)
    }

    println!("At {}:{}: {:?}", reader.get_line(), reader.get_col(), reader.get_err())
}
