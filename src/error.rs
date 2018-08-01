use nom;
use nom::types::CompleteStr;

use std::convert;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Parser error: {}", _0)]
    Parser(String),
    #[fail(display = "Syntax error: {}", msg)]
    Syntax { msg: String, stderr: String },
    #[fail(display = "{}: error: program must have an END statement \n", line_number)]
    MissingEnd { line_number: u16 },
}

impl<'a> convert::From<nom::Err<CompleteStr<'a>>> for Error {
    fn from(e: nom::Err<CompleteStr<'a>>) -> Self {
        Error::Parser(format!("{}", e))
    }
}
