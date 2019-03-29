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

impl From<basic::Error> for Output {
    fn from(e: basic::Error) -> Self {
        Self {
            stdout: String::new(),
            stderr: format!("{}", e),
        }
    }
}

#[wasm_bindgen]
pub fn execute(input: &str) -> Output {
    let mut stdin = std::io::empty();
    let mut stdout = Vec::new();
    let mut stderr = Vec::new();

    if let Err(e) = basic::execute(input, &mut stdin, &mut stdout, &mut stderr) {
        Output::from(e)
    } else {
        Output {
            stdout: String::from_utf8(stdout).unwrap(),
            stderr: String::from_utf8(stderr).unwrap(),
        }
    }
}
