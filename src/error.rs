use ast::StringVariable;

use nom;
use nom::types::CompleteStr;

use std::convert;
use std::fmt;

#[derive(Debug, Fail)]
pub enum Error {
    Parser(String),
    StatementsAfterEnd { line_numbers: Vec<u16> },
    MissingEnd { line_number: u16 },
    InvalidTabCall,
    UndefinedStringVariable(StringVariable),
}

impl<'a> convert::From<nom::Err<CompleteStr<'a>>> for Error {
    fn from(e: nom::Err<CompleteStr<'a>>) -> Self {
        Error::Parser(format!("{}", e))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Parser(ref s) => write!(f, "Parser error: {}", s),
            Error::StatementsAfterEnd { ref line_numbers } => {
                for line_number in line_numbers {
                    write!(f, "{}: error: line after an END statement \n", line_number)?;
                }
                Ok(())
            }
            Error::MissingEnd { line_number } => write!(
                f,
                "{}: error: program must have an END statement \n",
                line_number
            ),
            Error::InvalidTabCall => write!(f, "error: invalid TAB call"),
            Error::UndefinedStringVariable(ref variable) => {
                write!(f, "error: unknown string variable '{}$'", variable.0)
            }
        }
    }
}
