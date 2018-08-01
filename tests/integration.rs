extern crate basic;
extern crate diff;
extern crate failure;

use failure::Error;

use std::fs::File;
use std::io::Read;

fn read_to_string(path: &str) -> Result<String, Error> {
    let mut f = File::open(path)?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;
    Ok(String::from_utf8(buffer)?)
}

fn diff(from: &str, to: &str) -> String {
    let diff_lines: Vec<_> = diff::lines(from, to)
        .into_iter()
        .filter_map(|l| match l {
            diff::Result::Left(s) => Some(format!("+ {}", s)),
            diff::Result::Right(s) => Some(format!("- {}", s)),
            diff::Result::Both(_, _) => None,
        })
        .collect();
    diff_lines.join("\n")
}

#[test]
fn test_p001() {
    let input = read_to_string("tests/P001.BAS").expect("failed to read test program");
    let expected = read_to_string("tests/P001.ok").expect("failed to read expected output");

    let res = basic::execute(&input);
    assert!(res.is_ok());
    let output = res.unwrap();

    assert_eq!(output, expected, "\nDiff:\n{}\n", diff(&output, &expected));
}
