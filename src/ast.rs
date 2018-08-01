#[derive(Debug)]
pub enum Digit {
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

#[derive(Debug)]
pub struct Program {
    pub blocks: Vec<Block>,
}

#[derive(Debug)]
pub enum Block {
    Line {
        line_number: u16,
        statement: Statement,
    },
}

#[derive(Debug)]
pub enum Statement {
    Print(PrintStatement),
    End,
}

// 6. Constants

#[derive(Debug)]
pub enum Constant {
    Numeric(f64),
    String(StringConstant),
}

#[derive(Debug)]
pub struct StringConstant(pub String);

// 7. Variable
// TODO: numeric-array-element is missing

#[derive(Debug)]
pub enum NumericVariable {
    Simple { letter: char, digit: Option<Digit> },
}

#[derive(Debug)]
pub struct StringVariable {
    pub letter: char,
}

#[derive(Debug)]
pub enum Variable {
    Numeric(NumericVariable),
    String(StringVariable),
}

// 8. Expressions

#[derive(Debug)]
pub enum Expression {
    Numeric(NumericExpression),
    String(StringExpression),
}

#[derive(Debug)]
pub struct NumericExpression;

#[derive(Debug)]
pub enum StringExpression {
    Variable(StringVariable),
    Constant(StringConstant),
}

// 9. Implementation supplied functions

#[derive(Debug)]
pub enum Function {
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

#[derive(Debug)]
pub struct PrintStatement {
    pub list: Vec<PrintItem>,
}

#[derive(Debug)]
pub enum PrintItem {
    Expression(Expression),
    TabCall(NumericExpression),
}
