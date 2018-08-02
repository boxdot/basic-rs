#[macro_use]
extern crate nom;
#[macro_use]
extern crate failure;

use nom::types::CompleteStr;

mod ast;
mod error;
mod interpreter;
mod parser;

pub use error::Error;

pub fn execute(input: &str) -> Result<String, Error> {
    let (remaining, ast) = parser::program(CompleteStr(input))?;
    if !remaining.is_empty() {
        return Err(Error::Parser(format!(
            "failed to parse program fully, remaining statements:\n{}",
            remaining
        )));
    }
    let output = interpreter::evaluate(&ast?)?;
    Ok(output)
}
