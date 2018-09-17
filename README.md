# BASIC Interpreter ![Build Status][travis]

Implementation of BASIC interpreter in Rust. The implementation follows the [ECMA-55] specification.

This project is **WIP**:

▮▯▯▯▯▯▯▯▯▯▯▯▯ 8% (17/208 tests)

## Hot to implement a test

1. Download `https://raw.githubusercontent.com/jorgicor/bas55/master/tests/PXXX.{BAS, ok, eok}` to
  `tests/suite` and add

    ```rust
    test_program!(PXXX);
    ```

    to `tests/integration.rs`.
2. Run `cargo test PXXX`. The test will most likely fail.
3. Read the [ECMA-55] spec. 😀
3. Implement parsing in `src/parser.rs`.
3. Implement interpreter in `src/interpreter.rs`.
4. If needed, extend model in `src/ast.rs`.

[ECMA-55]: https://buraphakit.sourceforge.io/ECMA-55,1st_Edition,_January_1978.pdf
[travis]: https://travis-ci.com/boxdot/basic-rs.svg?branch=master
