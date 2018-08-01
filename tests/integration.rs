extern crate basic;
extern crate diff;
extern crate failure;

use basic::Error as BasicError;

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

fn run_and_compare_output(program_path: &str, expected_path: &str, expected_error: &str) {
    let input = read_to_string(program_path).expect("failed to read test program");
    let expected = read_to_string(expected_path).expect("failed to read expected output");
    let expected_error =
        read_to_string(expected_error).expect("failed to read expected error output");

    let res = basic::execute(&input);
    match res {
        Ok(output) => assert_eq!(output, expected, "\nDiff:\n{}\n", diff(&output, &expected)),
        Err(BasicError::Syntax { stderr, .. }) => {
            assert_eq!(
                stderr,
                expected_error,
                "\nDiff:\n{}\n",
                diff(&stderr, &expected_error)
            );
        }
        Err(e) => assert!(false, "Unexpected error: {}", e),
    }
}

#[test]
fn test_p001() {
    run_and_compare_output("tests/P001.BAS", "tests/P001.ok", "tests/P001.eok");
}

#[test]
fn test_p002() {
    run_and_compare_output("tests/P002.BAS", "tests/P002.ok", "tests/P002.eok");
}

#[test]
fn test_p003() {
    run_and_compare_output("tests/P003.BAS", "tests/P003.ok", "tests/P003.eok");
}
