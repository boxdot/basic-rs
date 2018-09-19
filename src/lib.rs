#[macro_use]
extern crate nom;
extern crate itertools;

use nom::types::CompleteStr;

mod ast;
mod error;
mod format;
mod interpreter;
mod parser;

pub use error::Error;

pub fn execute(input: &str) -> Result<(String, String), Error> {
    let res = parser::program(CompleteStr(input));
    match res {
        Ok((remaining, ast)) => if !remaining.is_empty() {
            Err(Error::Parser(format!(
                "failed to parse program fully, remaining statements:\n{}",
                remaining
            )))
        } else {
            interpreter::evaluate(&ast?)
        },
        Err(nom::Err::Failure(nom::simple_errors::Context::Code(
            source_code,
            nom::ErrorKind::Custom(err_code),
        ))) => {
            // Unfortunately, without enabling dynamic error handling in NOM, we don't
            // get precise context where the error happened. So, we just backtrack the line
            // where the remaining non-parsed source code begins in input.
            let line = backtrack_line(input, source_code);
            let stderr = parser::ErrorCode::from(err_code)
                .to_string(&line, source_code.lines().next().unwrap_or(""))
                .unwrap_or_else(|| format!("{}", err_code));
            Ok((String::new(), stderr))
        }
        Err(e) => {
            let stderr = format!("{}", e);
            Ok((String::new(), stderr))
        }
    }
}

/// Finds and returns the full line where `remaining` starts in `input`.
fn backtrack_line<'a>(input: &'a str, remaining: CompleteStr) -> &'a str {
    let num_remaining_lines = remaining.lines().count();
    input
        .lines()
        .rev()
        .skip(num_remaining_lines.checked_sub(1).unwrap_or(0))
        .next()
        .expect("logic error")
}
