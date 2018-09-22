#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_locate;
extern crate itertools;

mod ast;
mod error;
mod format;
mod interpreter;
mod parser;

use interpreter::Interpreter;
use nom::types::CompleteStr;

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
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            let ast = ast?;
            let interpreter = Interpreter::new(&ast, input);
            interpreter.evaluate(&mut stdout, &mut stderr)?;
            Ok((
                String::from_utf8(stdout).unwrap(),
                String::from_utf8(stderr).unwrap(),
            ))
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
        Err(e) => Err(Error::Parser(format!("parser error: {}", e))),
    }
}
