#[macro_use]
extern crate nom;

use nom::types::CompleteStr;

mod ast;
mod interpreter;
mod parser;

pub fn execute(input: &str) -> Result<String, nom::Err<CompleteStr>> {
    let (_remaining, ast) = parser::program(CompleteStr(input))?;
    let output = interpreter::evaluate(&ast);
    Ok(output)
}
