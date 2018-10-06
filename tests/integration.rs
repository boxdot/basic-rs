extern crate basic;
extern crate diff;

macro_rules! try_test_program {
    ($name:ident) => {
        #[test]
        #[allow(non_snake_case)]
        #[should_panic]
        fn $name() {
            run_and_compare_output(
                include_str!(concat!("suite/", stringify!($name), ".BAS")),
                include_str!(concat!("suite/", stringify!($name), ".ok")),
                include_str!(concat!("suite/", stringify!($name), ".eok")),
            );
        }
    };
}

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

macro_rules! test_program_3x {
    ($name:ident) => {
        #[test]
        #[allow(non_snake_case)]
        fn $name() {
            run_and_compare_output_3x(
                include_str!(concat!("suite/", stringify!($name), ".BAS")),
                include_str!(concat!("suite/", stringify!($name), ".ok")),
                include_str!(concat!("suite/", stringify!($name), ".eok")),
            );
        }
    };
}

fn run_and_compare_output(program: &str, expected_output: &str, expected_err_output: &str) {
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    let res = basic::execute(&program, &mut stdout, &mut stderr);

    let output = String::from_utf8(stdout).unwrap();
    let mut err_output = String::from_utf8(stderr).unwrap();
    if let Err(e) = res {
        err_output += &format!("{}", e);
    }

    if program.find("THIS TEST IS INFORMATIVE ONLY").is_some() {
        assert!(output.find("FAILED").is_none());
    } else {
        assert_eq!(
            output,
            expected_output,
            "\nDiff:\n{}\n",
            diff(&output, expected_output)
        );
    }

    assert_eq!(
        err_output,
        expected_err_output,
        "\nDiff:\n{}\n",
        diff(&err_output, &expected_err_output)
    );
}

fn run_and_compare_output_3x(program: &str, expected_output: &str, expected_err_output: &str) {
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    basic::execute(&program, &mut stdout, &mut stderr).expect("execute failed");
    let output = String::from_utf8(stdout).unwrap();
    let err_output = String::from_utf8(stderr).unwrap();

    assert_eq!(
        output,
        expected_output,
        "\nDiff:\n{}\n",
        diff(&output, expected_output)
    );

    assert_eq!(
        err_output,
        expected_err_output,
        "\nDiff:\n{}\n",
        diff(&err_output, &expected_err_output)
    );

    for _ in 1..3 {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();

        basic::execute(&program, &mut stdout, &mut stderr).expect("execute failed");

        let next_output = String::from_utf8(stdout).unwrap();
        let next_err_output = String::from_utf8(stderr).unwrap();

        assert_eq!(
            next_output,
            output,
            "\nDiff:\n{}\n",
            diff(&next_output, &output)
        );

        assert_eq!(
            next_err_output,
            err_output,
            "\nDiff:\n{}\n",
            diff(&next_err_output, &err_output)
        );
    }
}

fn diff(from: &str, to: &str) -> String {
    let diff_lines: Vec<_> = diff::lines(from, to)
        .into_iter()
        .filter_map(|l| match l {
            diff::Result::Left(s) => Some(format!("+ {}", s)),
            diff::Result::Right(s) => Some(format!("- {}", s)),
            diff::Result::Both(_, _) => None,
        }).collect();
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
test_program!(P016);
test_program!(P017);
test_program!(P018);
test_program!(P019);
test_program!(P020);
test_program!(P021);
test_program!(P022);
test_program!(P023);
test_program!(P024);
test_program!(P025);
test_program!(P026);
test_program!(P027);
test_program!(P028);
test_program!(P029);
test_program!(P030);
test_program!(P031);
test_program!(P032);
test_program!(P033);
test_program!(P034);
test_program!(P035);
test_program!(P036);
test_program!(P037);
test_program!(P038);
test_program!(P039);
test_program!(P040);
test_program!(P041);
test_program!(P042);
test_program!(P043);
test_program!(P044);
test_program!(P045);
test_program!(P046);
test_program!(P047);
test_program!(P048);
test_program!(P049);
test_program!(P050);
test_program!(P051);
test_program!(P052);
test_program!(P053);
test_program!(P054);
try_test_program!(P055);
try_test_program!(P056);
try_test_program!(P057);
try_test_program!(P058);
try_test_program!(P059);
try_test_program!(P060);
try_test_program!(P061);
try_test_program!(P062);
try_test_program!(P063);
try_test_program!(P064);
try_test_program!(P065);
try_test_program!(P066);
try_test_program!(P067);
try_test_program!(P068);
try_test_program!(P069);
try_test_program!(P070);
try_test_program!(P071);
try_test_program!(P072);
try_test_program!(P073);
try_test_program!(P074);
try_test_program!(P075);
try_test_program!(P076);
try_test_program!(P077);
try_test_program!(P078);
try_test_program!(P079);
try_test_program!(P080);
try_test_program!(P081);
try_test_program!(P082);
try_test_program!(P083);
try_test_program!(P084);
try_test_program!(P085);
try_test_program!(P086);
test_program!(P087);
test_program!(P088);
test_program!(P089);
test_program!(P090);
try_test_program!(P091);
try_test_program!(P092);
try_test_program!(P093);
try_test_program!(P094);
test_program!(P095);
test_program!(P096);
try_test_program!(P097);
try_test_program!(P098);
try_test_program!(P099);
test_program!(P100);
try_test_program!(P101);
try_test_program!(P102);
try_test_program!(P103);
try_test_program!(P104);
test_program!(P105);
test_program!(P106);
try_test_program!(P107);
try_test_program!(P108);
try_test_program!(P109);
try_test_program!(P110);
try_test_program!(P111);
try_test_program!(P112);
try_test_program!(P113);
test_program!(P114);
try_test_program!(P115);
test_program!(P116);
test_program!(P117);
test_program!(P118);
test_program!(P119);
test_program!(P120);
test_program!(P121);
test_program!(P122);
test_program!(P123);
test_program!(P124);
test_program!(P125);
test_program!(P126);
test_program!(P127);
test_program!(P128);
test_program!(P129);
test_program_3x!(P130);
try_test_program!(P131);
try_test_program!(P132);
try_test_program!(P133);
try_test_program!(P134);
try_test_program!(P135);
try_test_program!(P136);
try_test_program!(P137);
try_test_program!(P138);
try_test_program!(P139);
try_test_program!(P140);
try_test_program!(P141);
test_program!(P142);
try_test_program!(P143);
try_test_program!(P144);
try_test_program!(P145);
try_test_program!(P146);
try_test_program!(P147);
try_test_program!(P148);
test_program!(P149);
try_test_program!(P150);
try_test_program!(P151);
try_test_program!(P152);
try_test_program!(P153);
try_test_program!(P154);
try_test_program!(P155);
try_test_program!(P156);
try_test_program!(P157);
try_test_program!(P158);
try_test_program!(P159);
try_test_program!(P160);
try_test_program!(P161);
try_test_program!(P162);
try_test_program!(P163);
try_test_program!(P164);
try_test_program!(P165);
try_test_program!(P166);
try_test_program!(P167);
try_test_program!(P168);
try_test_program!(P169);
try_test_program!(P170);
try_test_program!(P171);
test_program!(P172);
test_program!(P173);
try_test_program!(P174);
try_test_program!(P175);
test_program!(P176);
try_test_program!(P177);
try_test_program!(P178);
test_program!(P179);
test_program!(P180);
try_test_program!(P181);
try_test_program!(P182);
try_test_program!(P183);
try_test_program!(P184);
try_test_program!(P185);
test_program!(P186);
try_test_program!(P187);
try_test_program!(P188);
try_test_program!(P189);
try_test_program!(P190);
try_test_program!(P191);
try_test_program!(P192);
try_test_program!(P193);
try_test_program!(P194);
try_test_program!(P195);
test_program!(P196);
try_test_program!(P197);
try_test_program!(P198);
try_test_program!(P199);
try_test_program!(P200);
try_test_program!(P201);
test_program!(P202);
try_test_program!(P203);
try_test_program!(P204);
try_test_program!(P205);
try_test_program!(P206);
try_test_program!(P207);
try_test_program!(P208);
test_program!(printab);
try_test_program!(printspc);
try_test_program!(table);
test_program!(truend);
