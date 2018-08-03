extern crate basic;

use std::env;
use std::fs::read_to_string;

fn main() -> Result<(), basic::Error> {
    let filename = env::args()
        .nth(1)
        .expect("missing obligatory argument 'FILENAME'");
    let input = read_to_string(filename).expect("failed to read test program");
    println!("{}", basic::execute(&input)?);
    Ok(())
}
