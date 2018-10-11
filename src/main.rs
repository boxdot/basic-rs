extern crate basic;

use std::env;
use std::fs::read_to_string;
use std::io;

fn main() {
    let filename = env::args()
        .nth(1)
        .expect("missing obligatory argument 'FILENAME'");
    let input = read_to_string(filename).expect("failed to read test program");
    let res = basic::execute(
        &input,
        &mut io::BufReader::new(io::stdin()),
        &mut io::stdout(),
        &mut io::stderr(),
    );
    if let Err(e) = res {
        eprintln!("{}", e);
    }
}
