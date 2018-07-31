#![allow(dead_code)]

enum Digit {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
}

struct Program {
    blocks: Vec<Block>,
}

enum Block {
    Line {
        line_number: usize,
        statement: Statement,
    },
}

enum Statement {
    Print(PrintStatement),
}

// 6. Constants

enum Constant {
    Numeric(f64),
    String(StringConstant),
}

struct StringConstant {
    value: String,
}

// 7. Variable
// TODO: numeric-array-element is missing

enum NumericVariable {
    Simple { letter: char, digit: Option<Digit> },
}

struct StringVariable {
    letter: char,
}

enum Variable {
    Numeric(NumericVariable),
    String(StringVariable),
}

// 8. Expressions

enum Expression {
    Numeric(NumericExpression),
    String(StringExpression),
}

struct NumericExpression;

enum StringExpression {
    StringVariable(StringVariable),
    StringConstant(StringConstant),
}

// 9. Implementation supplied functions

enum Function {
    Abs(f64),
    Atn(f64),
    Cos(f64),
    Exp(f64),
    Int(f64),
    Log(f64),
    Rnd,
    Sgn(f64),
    Sin(f64),
    Sqr(f64),
    Tan(f64),
}

// 14. Print statement

struct PrintStatement {
    list: Vec<PrintItem>,
}

enum PrintItem {
    Expression(Expression),
    TabCall(NumericExpression),
}

fn main() {
    println!("Hello, world!");
}
