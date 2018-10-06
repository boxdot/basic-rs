# BASIC Interpreter ![Build Status][travis]

Implementation of BASIC interpreter in Rust. The implementation follows the [ECMA-55] specification.

This project is **WIP**:

![test coverage](https://img.shields.io/badge/style-92%2F212%20tests-blue.svg?longCache=true&label=Minimal%20basic%20test%20coverage)

1. Pick any tests marked with `try_test_program` in [tests/integration.rs](tests/integration.rs).
2. Change the test to use the `test_program` macro.
3. Run `cargo test PXXX` where `XXX` is one of the tests you want to fix.
4. Read the [ECMA-55] spec. 😀
5. Implement parsing in `src/parser.rs`.
6. Implement interpreter in `src/interpreter.rs`.
7. If needed, extend model in `src/ast.rs`.
8. Run `cargo test` to see if you made any other tests pass!
9. Profit!

[ECMA-55]: https://buraphakit.sourceforge.io/ECMA-55,1st_Edition,_January_1978.pdf
[travis]: https://travis-ci.com/boxdot/basic-rs.svg?branch=master
