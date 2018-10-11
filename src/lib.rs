#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_locate;
extern crate itertools;
extern crate rand;

mod ast;
mod error;
mod format;
mod interpreter;
mod parser;

use interpreter::Interpreter;
use nom::simple_errors::Context;
use nom::types::CompleteStr;

pub use error::Error;

use std::io::{BufRead, Write};

pub fn execute<R: BufRead, W: Write, V: Write>(
    input: &str,
    stdin: &mut R,
    stdout: &mut W,
    stderr: &mut V,
) -> Result<(), Error> {
    let res = parser::program(parser::Span::new(CompleteStr(input)));
    match res {
        Ok((remaining, ast)) => {
            if !remaining.fragment.is_empty() {
                write!(stderr, "{}", error::format_remaining(&remaining.fragment)?);
                Ok(())
            } else {
                let ast = ast?;
                let interpreter = Interpreter::new(&ast, input);
                interpreter.evaluate(stdin, stdout, stderr)?;
                Ok(())
            }
        }
        Err(nom::Err::Failure(Context::Code(span, nom::ErrorKind::Custom(err_code)))) => {
            let err_output = parser::ErrorCode::from(err_code).to_string(&span, input);
            write!(stderr, "{}", err_output);
            Ok(())
        }
        Err(e) => Err(Error::Parser(format!("parser error: {}", e))),
    }
}
