mod ast;
mod error;
mod format;
mod interpreter;
mod parser2;

pub use crate::error::Error;
use crate::interpreter::Interpreter;

use std::io::{BufRead, Write};

pub fn execute<R: BufRead, W: Write, V: Write>(
    input: &str,
    stdin: &mut R,
    stdout: &mut W,
    stderr: &mut V,
) -> Result<(), Error> {
    let (remaining, ast) = parser2::program(input)?;
    if !remaining.is_empty() {
        write!(stderr, "{}", error::format_remaining(&remaining)?)?;
    } else {
        let ast = ast?;
        let interpreter = Interpreter::new(&ast, input);
        interpreter.evaluate(stdin, stdout, stderr)?;
    }
    Ok(())
}
