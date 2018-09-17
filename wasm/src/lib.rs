extern crate basic;
extern crate cfg_if;
extern crate wasm_bindgen;

use basic::Error;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Debug, Default)]
pub struct Output {
    stdout: String,
    stderr: String,
}

#[wasm_bindgen]
impl Output {
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }

    pub fn stderr(&self) -> String {
        self.stderr.clone()
    }
}

impl From<(String, String)> for Output {
    fn from((stdout, stderr): (String, String)) -> Self {
        Self { stdout, stderr }
    }
}

impl From<Error> for Output {
    fn from(e: Error) -> Self {
        Self {
            stdout: String::new(),
            stderr: format!("{}", e),
        }
    }
}

#[wasm_bindgen]
pub fn execute(input: &str) -> Output {
    basic::execute(input)
        .map(Output::from)
        .unwrap_or_else(Output::from)
}
