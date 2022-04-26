use std::io::{Read, Write};
use crate::Parser;

struct SmtClient<R: Read, W: Write> {
    reader: Parser<R>,
    writer: W,
}

impl<R: Read, W: Write> SmtClient<R, W> {
    pub fn new(reader: R, writer: W) -> Self {
        Self { reader: Parser::new(reader), writer }
    }
}