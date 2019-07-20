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
    parser2::program(input).and_then(|ast| {
        let interpreter = Interpreter::new(&ast, input);
        interpreter.evaluate(stdin, stdout, stderr)
    })
}
