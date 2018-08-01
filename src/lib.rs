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

fn handle_remaining(remaining: CompleteStr) -> Result<(), Error> {
    if remaining.is_empty() {
        return Ok(());
    }

    let stderr: Result<Vec<String>, _> = remaining
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| -> Result<String, Error> {
            let (_, line_number) = parser::line_number(CompleteStr(line))?;
            Ok(format!(
                "{}: error: line after an END statement ",
                line_number
            ))
        })
        .collect();
    let stderr = stderr?;
    return Err(Error::Syntax {
        msg: "extra lines after an END statement".into(),
        stderr: stderr.join("\n") + "\n",
    });
}

pub fn execute(input: &str) -> Result<String, Error> {
    let (remaining, ast) = parser::program(CompleteStr(input))?;
    handle_remaining(remaining)?;
    let output = interpreter::evaluate(&ast?);
    Ok(output)
}
