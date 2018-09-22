use ast::{NumericVariable, StringVariable};

use nom;
use nom::types::CompleteStr;

use std::convert;
use std::fmt;

#[derive(Debug)]
pub enum Error {
    Parser(String),
    StatementsAfterEnd {
        line_numbers: Vec<u16>,
    },
    MissingEnd {
        src_line_number: u16,
    },
    InvalidTabCall,
    DuplicateLineNumber {
        line_number: u16,
    },
    UndefinedLineNumber {
        src_line_number: u16,
        line_number: u16,
        statement_source: String,
    },
    UnexpectedReturn {
        src_line_number: u16,
    },
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
                    writeln!(f, "{}: error: line after an END statement ", line_number)?;
                }
                Ok(())
            }
            Error::MissingEnd { src_line_number } => writeln!(
                f,
                "{}: error: program must have an END statement ",
                src_line_number
            ),
            Error::InvalidTabCall => write!(f, "error: invalid TAB call"),
            Error::DuplicateLineNumber { line_number } => {
                write!(f, "error: duplicate line number {}", line_number)
            }
            Error::UndefinedLineNumber {
                src_line_number,
                line_number,
                ref statement_source,
            } => write!(
                f,
                "{}: error: non-existing line number \n {}\n{:cursor$}^\n",
                src_line_number,
                statement_source,
                "",
                cursor = statement_source
                    .find(&format!("{}", line_number))
                    .unwrap_or(0)
                    + 1
            ),
            Error::UnexpectedReturn { src_line_number } => {
                write!(f, "{}: error: unexpected return\n", src_line_number)
            }
        }
    }
}
