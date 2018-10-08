use nom;
use nom::simple_errors::Context;
use nom::types::CompleteStr;

use ast::NextLine;
use parser::{self, Span};

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
    StackUnderflow {
        src_line_number: u16,
    },
    FracPowOfNegValue {
        src_line_number: u16,
        value: f64,
        exp: f64,
    },
    InsufficientData {
        src_line_number: u16,
    },
    ReadDatatypeMismatch {
        src_line_number: u16,
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
    ControlVariableReuse {
        src_line_number: u16,
        outer_line_number: u16,
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
            Error::StackUnderflow { src_line_number } => {
                write!(f, "{}: error: stack underflow \n", src_line_number)
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
            Error::InsufficientData { src_line_number } => write!(
                f,
                "{}: error: insufficient data for READ \n",
                src_line_number
            ),
            Error::ReadDatatypeMismatch { src_line_number } => write!(
                f,
                "{}: error: reading string into numeric variable \n",
                src_line_number
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
            Error::ControlVariableReuse {
                src_line_number,
                outer_line_number,
            } => write!(
                f,
                "{}: error: FOR uses the same variable as outer FOR at line {}\n",
                src_line_number, outer_line_number
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

pub fn format_remaining<'a>(remaining: &'a str) -> Result<String, Error> {
    let failed_line = remaining.lines().next().unwrap();
    let failed_span = Span::new(CompleteStr(failed_line));

    // special case: unexpected NEXT statement => no corresponding FOR block
    if let Ok((_, NextLine { line_number, .. })) = parser::next_line(failed_span) {
        return Ok(format!("{}: error: NEXT without FOR \n", line_number));
    }

    // general case
    // extract fragment where the parser failed from all possible errors
    let failed_fragment = match parser::line(failed_span) {
        Ok((span, _)) => span.fragment,
        Err(nom::Err::Error(Context::Code(span, _))) => span.fragment,
        Err(nom::Err::Failure(Context::Code(span, _))) => span.fragment,
        Err(e) => return Err(Error::from(e)),
    };

    let mut parts = failed_line.splitn(2, ' ');
    let line_number = parts.next().unwrap();
    let statement = parts.next().unwrap();
    let failed_pos = statement.find(failed_fragment.as_ref()).unwrap_or(0);
    Ok(format!(
        "{}: error: syntax error \n {}\n {:cursor$}^\n",
        line_number,
        statement,
        "",
        cursor = failed_pos + 1
    ))
}
