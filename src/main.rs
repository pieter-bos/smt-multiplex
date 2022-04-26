// TODO remove once everything is actually used.
#![allow(dead_code)]

mod lexer;
mod parser;
mod uninterpreted_ast;
mod ast;

mod display;

mod smt_client;
mod smt_server;

use std::fs::File;
use std::io::BufReader;
use bigdecimal::BigDecimal;
use lexer::*;
use parser::*;

fn main() {
    let x: BigDecimal = BigDecimal::from(1) / 10;
    println!("{}", x);

    let f = File::open("/home/pieter/vercors/tmp/logfile-00.smt2").unwrap();
    let mut reader = ScriptParser::new(BufReader::new(f));

    for command in &mut reader {
        println!("{:?}", command);
    }

    if let Some(e) = reader.take_err() {
        println!("{}", e);
    }
}