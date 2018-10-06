use nom;
use nom::simple_errors::Context;

use parser::Span;

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
    FracPowOfNegValue {
        src_line_number: u16,
        value: f64,
        exp: f64,
    },
    MissingData {
        src_line_number: u16,
    },
    ReadDatatypeMismatch {
        src_line_number: u16,
        data_pointer: u16,
    },
    FunctionDomainError {
        src_line_number: u16,
        function: String,
        arg: f64,
    },
    InvalidControlVariable {
        src_line_number: u16,
        control_variable: String,
    },
    JumpIntoFor {
        src_line_number: u16,
    },
    IndexOutOfRange {
        src_line_number: u16,
    },
}

impl<'a> convert::From<nom::Err<Span<'a>>> for Error {
    fn from(e: nom::Err<Span<'a>>) -> Self {
        match e {
            nom::Err::Incomplete(needed) => {
                Error::Parser(format!("incomplete input: {:?}", needed))
            }
            nom::Err::Error(Context::Code(span, _)) => Error::Parser(format!("{}", span.fragment)),
            nom::Err::Failure(Context::Code(span, _)) => {
                Error::Parser(format!("{}", span.fragment))
            }
        }
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
            } => {
                // find the position of a line number in the statement source code
                let line_number_position = statement_source
                    .find(&format!("{}", line_number))
                    .unwrap_or(0)
                    + 1;
                write!(
                    f,
                    "{}: error: non-existing line number \n {}\n{:cursor$}^\n",
                    src_line_number,
                    statement_source,
                    "",
                    cursor = line_number_position
                )
            }
            Error::UnexpectedReturn { src_line_number } => {
                write!(f, "{}: error: unexpected return\n", src_line_number)
            }
            Error::FracPowOfNegValue {
                src_line_number,
                value,
                exp,
            } => write!(
                f,
                "{}: error: negative value raised to non-integral value ({} ^ {})\n",
                src_line_number, value, exp
            ),
            Error::MissingData { src_line_number } => {
                write!(f, "{}: error: missing data\n", src_line_number)
            }
            Error::ReadDatatypeMismatch {
                src_line_number,
                data_pointer,
            } => write!(
                f,
                "{}: error: mismatch between read statement and DATA at position {}",
                src_line_number, data_pointer
            ),
            Error::FunctionDomainError {
                src_line_number,
                ref function,
                arg,
            } => write!(
                f,
                "{}: error: function domain error {}({})\n",
                src_line_number, function, arg
            ),
            Error::InvalidControlVariable {
                src_line_number,
                ref control_variable,
            } => write!(
                f,
                "{}: error: invalid control variable {}\n",
                src_line_number, control_variable
            ),
            Error::JumpIntoFor { src_line_number } => {
                write!(f, "{}: error: jump into FOR block \n", src_line_number)
            }
            Error::IndexOutOfRange { src_line_number } => {
                write!(f, "{}: error: index out of range \n", src_line_number)
            }
        }
    }
}
