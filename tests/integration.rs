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

fn run_and_compare_output(program_path: &str, expected_path: &str, expected_error: &str) {
    let input = read_to_string(program_path).expect("failed to read test program");
    let expected = read_to_string(expected_path).expect("failed to read expected output");
    let expected_error =
        read_to_string(expected_error).expect("failed to read expected error output");

    let res = basic::execute(&input);
    match res {
        Ok((output, err_output)) => {
            assert_eq!(output, expected, "\nDiff:\n{}\n", diff(&output, &expected));
            assert_eq!(
                err_output,
                expected_error,
                "\nDiff:\n{}\n",
                diff(&err_output, &expected_error)
            );
        }
        Err(e) => {
            let stderr = format!("{}", e);
            assert_eq!(
                stderr,
                expected_error,
                "\nDiff:\n{}\n",
                diff(&stderr, &expected_error)
            );
        }
    }
}

#[test]
fn test_p001() {
    run_and_compare_output(
        "tests/suite/P001.BAS",
        "tests/suite/P001.ok",
        "tests/suite/P001.eok",
    );
}

#[test]
fn test_p002() {
    run_and_compare_output(
        "tests/suite/P002.BAS",
        "tests/suite/P002.ok",
        "tests/suite/P002.eok",
    );
}

#[test]
fn test_p003() {
    run_and_compare_output(
        "tests/suite/P003.BAS",
        "tests/suite/P003.ok",
        "tests/suite/P003.eok",
    );
}

#[test]
fn test_p004() {
    run_and_compare_output(
        "tests/suite/P004.BAS",
        "tests/suite/P004.ok",
        "tests/suite/P004.eok",
    );
}

#[test]
fn test_p005() {
    run_and_compare_output(
        "tests/suite/P005.BAS",
        "tests/suite/P005.ok",
        "tests/suite/P005.eok",
    );
}

#[test]
fn test_p006() {
    run_and_compare_output(
        "tests/suite/P006.BAS",
        "tests/suite/P006.ok",
        "tests/suite/P006.eok",
    );
}

#[test]
fn test_p007() {
    run_and_compare_output(
        "tests/suite/P007.BAS",
        "tests/suite/P007.ok",
        "tests/suite/P007.eok",
    );
}

#[test]
fn test_p008() {
    run_and_compare_output(
        "tests/suite/P008.BAS",
        "tests/suite/P008.ok",
        "tests/suite/P008.eok",
    );
}

#[test]
fn test_p009() {
    run_and_compare_output(
        "tests/suite/P009.BAS",
        "tests/suite/P009.ok",
        "tests/suite/P009.eok",
    );
}
