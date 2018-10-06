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

use std::io::Write;

pub fn execute<W: Write, V: Write>(
    input: &str,
    stdout: &mut W,
    stderr: &mut V,
) -> Result<(), Error> {
    let res = parser::program(parser::Span::new(CompleteStr(input)));
    match res {
        Ok((remaining, ast)) => {
            if !remaining.fragment.is_empty() {
                write!(stderr, "{}", syntax_error_with_cursor(&remaining.fragment)?);
                Ok(())
            } else {
                let ast = ast?;
                let interpreter = Interpreter::new(&ast, input);
                interpreter.evaluate(stdout, stderr)?;
                Ok(())
            }
        }
        Err(nom::Err::Failure(Context::Code(span, nom::ErrorKind::Custom(err_code)))) => {
            let line = input.lines().nth(span.line as usize - 1).unwrap();
            let err_output = parser::ErrorCode::from(err_code)
                .to_string(line, &span.fragment.lines().next().unwrap())
                .unwrap_or_else(|| format!("{}", err_code));
            write!(stderr, "{}", err_output);
            Ok(())
        }
        Err(e) => Err(Error::Parser(format!("parser error: {}", e))),
    }
}

fn syntax_error_with_cursor<'a>(remaining: &'a str) -> Result<String, Error> {
    let failed_line = remaining.lines().next().unwrap();
    let failed_span = parser::Span::new(CompleteStr(failed_line));

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
