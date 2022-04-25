mod lexer;
mod parser;
mod uninterpreted_ast;
mod display;

use std::fs::File;
use std::io::BufReader;
use lexer::*;
use parser::*;

fn main() {
    let f = File::open("/home/pieter/vercors/tmp/logfile-00.smt2").unwrap();
    let mut reader = ScriptParser::new(BufReader::new(f));

    for command in &mut reader {
        println!("{:?}", command);
    }

    if let Some(e) = reader.take_err() {
        println!("{}", e);
    }
}
