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
use nom::simple_errors::Context;
use nom::types::CompleteStr;

pub use error::Error;

pub fn execute(input: &str) -> Result<(String, String), Error> {
    let res = parser::program(parser::Span::new(CompleteStr(input)));
    match res {
        Ok((remaining, ast)) => {
            if !remaining.fragment.is_empty() {
                Ok((
                    String::new(),
                    syntax_error_with_cursor(&remaining.fragment)?,
                ))
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
            }
        }
        Err(nom::Err::Failure(Context::Code(span, nom::ErrorKind::Custom(err_code)))) => {
            let line = input.lines().nth(span.line as usize - 1).unwrap();
            let stderr = parser::ErrorCode::from(err_code)
                .to_string(line, &span.fragment.lines().next().unwrap())
                .unwrap_or_else(|| format!("{}", err_code));
            Ok((String::new(), stderr))
        }
        Err(e) => Err(Error::Parser(format!("parser error: {}", e))),
    }
}

fn syntax_error_with_cursor<'a>(remaining: &'a str) -> Result<String, Error> {
    let failed_line = remaining.lines().next().unwrap();
    let failed_span = parser::Span::new(CompleteStr(failed_line));
    let (remaining, _) = parser::block(failed_span)?;
    let mut parts = failed_line.splitn(2, ' ');
    let line_number = parts.next().unwrap();
    let statement = parts.next().unwrap();
    let failed_pos = statement.find(remaining.fragment.as_ref()).unwrap();
    Ok(format!(
        "{}: error: syntax error \n {}\n {:cursor$}^\n",
        line_number,
        statement,
        "",
        cursor = failed_pos + 1
    ))
}
