use nom;
use nom::simple_errors::Context;
use nom::types::CompleteStr;

use crate::ast::NextLine;
use crate::parser::{self, Span};

use std::convert;
use std::fmt;
use std::io;

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
    ArrayIndexOutOfRange {
        src_line_number: u16,
        array: String,
    },
    TypeMismatch {
        src_line_number: u16,
        variable: String,
        info: String,
    },
    Redimensioned {
        src_line_number: u16,
        variable: char,
        bounds: (u64, Option<u64>),
    },
    InvalidOptionBase {
        src_line_number: u16,
        base: usize,
    },
    UndefinedFunction {
        src_line_number: u16,
        name: char,
        statement_source: String,
    },
    InsufficientInput {
        src_line_number: u16,
    },
    InvalidDimSubscript {
        src_line_number: u16,
        subscript: usize,
        statement_source: String,
    },
    IoError(io::Error),
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

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
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
                writeln!(
                    f,
                    "{}: error: non-existing line number \n {}\n{:cursor$}^",
                    src_line_number,
                    statement_source,
                    "",
                    cursor = line_number_position
                )
            }
            Error::StackUnderflow { src_line_number } => {
                writeln!(f, "{}: error: stack underflow ", src_line_number)
            }
            Error::FracPowOfNegValue {
                src_line_number,
                value,
                exp,
            } => writeln!(
                f,
                "{}: error: negative value raised to non-integral value ({} ^ {})",
                src_line_number, value, exp
            ),
            Error::InsufficientData { src_line_number } => {
                writeln!(f, "{}: error: insufficient data for READ ", src_line_number)
            }
            Error::ReadDatatypeMismatch { src_line_number } => writeln!(
                f,
                "{}: error: reading string into numeric variable ",
                src_line_number
            ),
            Error::FunctionDomainError {
                src_line_number,
                ref function,
                arg,
            } => writeln!(
                f,
                "{}: error: function domain error {}({})",
                src_line_number, function, arg
            ),
            Error::InvalidControlVariable {
                src_line_number,
                ref control_variable,
            } => writeln!(
                f,
                "{}: error: invalid control variable {}",
                src_line_number, control_variable
            ),
            Error::ControlVariableReuse {
                src_line_number,
                outer_line_number,
            } => writeln!(
                f,
                "{}: error: FOR uses the same variable as outer FOR at line {}",
                src_line_number, outer_line_number
            ),
            Error::JumpIntoFor { src_line_number } => {
                writeln!(f, "{}: error: jump into FOR block ", src_line_number)
            }
            Error::IndexOutOfRange { src_line_number } => {
                writeln!(f, "{}: error: index out of range ", src_line_number)
            }
            Error::ArrayIndexOutOfRange {
                src_line_number,
                ref array,
            } => writeln!(
                f,
                "{}: error: index out of range {}",
                src_line_number, array
            ),
            Error::TypeMismatch {
                src_line_number,
                ref variable,
                ref info,
            } => write!(
                f,
                "{}: error: type mismatch for variable {}\n info: {}",
                src_line_number, variable, info
            ),
            Error::Redimensioned {
                src_line_number,
                variable,
                bounds,
            } => {
                let bounds = if let (dim1, Some(dim2)) = bounds {
                    format!("({},{})", dim1, dim2)
                } else {
                    format!("({})", bounds.0)
                };
                writeln!(
                    f,
                    "{}: error: redimensioned variable {variable}\n DIM {variable}{bounds}\n     ^",
                    src_line_number,
                    variable = variable,
                    bounds = bounds
                )
            }
            Error::InvalidOptionBase {
                src_line_number,
                base,
            } => write!(
                f,
                "{}: error: OPTION used after arrays used or DIM \n OPTION BASE {}\n ^\n",
                src_line_number, base,
            ),
            Error::UndefinedFunction {
                src_line_number,
                name,
                ref statement_source,
            } => {
                let fn_name = format!("FN{}", name);
                let cursor = statement_source
                    .find(&fn_name)
                    .and_then(|pos| {
                        // search for the second position, since the first is the definition of
                        // the function
                        let start = pos + fn_name.len();
                        statement_source[start..]
                            .find(&fn_name)
                            .map(|pos| pos + start)
                    })
                    .unwrap_or(0);
                writeln!(
                    f,
                    "{}: error: undefined function {}\n {}\n{:cursor$}^",
                    src_line_number,
                    fn_name,
                    statement_source,
                    "",
                    cursor = cursor + 1
                )
            }
            Error::InsufficientInput { src_line_number } => {
                writeln!(f, "{}: error: insufficient INPUT", src_line_number,)
            }
            Error::InvalidDimSubscript {
                src_line_number,
                subscript,
                ref statement_source,
            } => {
                let subscript = format!("{}", subscript);
                let cursor = statement_source.find(&subscript).unwrap_or(0);
                writeln!(
                    f,
                    "{}: error: invalid DIM subscript \n {}\n{:cursor$}^",
                    src_line_number,
                    statement_source,
                    "",
                    cursor = cursor + 1
                )
            }
            Error::IoError(ref e) => write!(f, "{}", e),
        }
    }
}

pub fn format_remaining(remaining: &'_ str) -> Result<String, Error> {
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
