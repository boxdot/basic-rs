#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_locate;
extern crate itertools;

use nom::types::CompleteStr;

mod ast;
mod error;
mod format;
mod interpreter;
mod parser;

pub use error::Error;

pub fn execute(input: &str) -> Result<(String, String), Error> {
    let res = parser::program(parser::Span::new(CompleteStr(input)));
    match res {
        Ok((remaining, ast)) => if !remaining.fragment.is_empty() {
            Err(Error::Parser(format!(
                "failed to parse program fully, remaining statements:\n{}",
                remaining.fragment
            )))
        } else {
            interpreter::evaluate(&ast?)
        },
        Err(nom::Err::Failure(nom::simple_errors::Context::Code(
            position,
            nom::ErrorKind::Custom(err_code),
        ))) => {
            let line = input.lines().nth(position.line as usize - 1).unwrap();
            let stderr = parser::ErrorCode::from(err_code)
                .to_string(line, &position.fragment.lines().next().unwrap())
                .unwrap_or_else(|| format!("{}", err_code));
            Ok((String::new(), stderr))
        }
        Err(e) => {
            let stderr = format!("{}", e);
            Ok((String::new(), stderr))
        }
    }
}
