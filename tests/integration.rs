extern crate basic;
extern crate diff;

macro_rules! test_program {
    ($name:ident) => {
        #[test]
        #[allow(non_snake_case)]
        fn $name() {
            run_and_compare_output(
                include_str!(concat!("suite/", stringify!($name), ".BAS")),
                include_str!(concat!("suite/", stringify!($name), ".ok")),
                include_str!(concat!("suite/", stringify!($name), ".eok")),
            );
        }
    };
}

fn run_and_compare_output(program: &str, expected_output: &str, expected_err_output: &str) {
    let res = basic::execute(&program);
    match res {
        Ok((output, err_output)) => {
            assert_eq!(
                output,
                expected_output,
                "\nDiff:\n{}\n",
                diff(&output, &expected_output)
            );
            assert_eq!(
                err_output,
                expected_err_output,
                "\nDiff:\n{}\n",
                diff(&err_output, &expected_err_output)
            );
        }
        Err(e) => {
            let stderr = format!("{}", e);
            assert_eq!(
                stderr,
                expected_err_output,
                "\nDiff:\n{}\n",
                diff(&stderr, &expected_err_output)
            );
        }
    }
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

test_program!(P001);
test_program!(P002);
test_program!(P003);
test_program!(P004);
test_program!(P005);
test_program!(P006);
test_program!(P007);
test_program!(P008);
test_program!(P009);
test_program!(P010);
test_program!(P011);
test_program!(P012);
test_program!(P013);
test_program!(P014);
test_program!(P015);
