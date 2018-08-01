#[macro_use]
extern crate nom;

use nom::types::CompleteStr;

mod ast;
mod parser;

pub fn execute(input: &str) -> Result<String, ()> {
    let res = parser::program(CompleteStr(input));
    assert!(res.is_ok());
    println!("{:#?}", res);
    Err(())
}
