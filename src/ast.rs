use error::Error;
use parser::Span;

use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops;

#[derive(Debug)]
pub struct Program<'a> {
    pub blocks: Vec<Block<'a>>,
    block_index: HashMap<u16, usize>,
    pub datum: Vec<Expression>,
}

impl<'a> Program<'a> {
    pub fn new(blocks: Vec<Block>) -> Result<Program, Error> {
        let end_statement_pos = blocks.iter().position(|b| match b {
            Block::Line {
                statement: Statement::End,
                ..
            } => true,
            _ => false,
        });

        if let Some(end_statement_pos) = end_statement_pos {
            if end_statement_pos + 1 != blocks.len() {
                let line_numbers =
                    blocks
                        .into_iter()
                        .skip(end_statement_pos + 1)
                        .map(|b| match b {
                            Block::Line { line_number, .. } => line_number,
                        });
                Err(Error::StatementsAfterEnd {
                    line_numbers: line_numbers.collect(),
                })
            } else {
                let program = Self::build_program(blocks)?;
                Ok(program)
            }
        } else {
            Err(Error::MissingEnd {
                src_line_number: blocks
                    .last()
                    .map(|b| match b {
                        Block::Line { line_number, .. } => *line_number,
                    }).unwrap_or(0u16),
            })
        }
    }

    pub fn first_block(&self) -> &Block {
        self.blocks.first().expect("logic error")
    }

    pub fn next_block<'b>(&'a self, block: &'b Block) -> &'a Block {
        let line_number = match block {
            Block::Line { line_number, .. } => line_number,
        };
        let index = self.block_index.get(line_number).expect("logic error");
        &self.blocks[index + 1]
    }

    pub fn get_block_by_line_number(&self, line_number: u16) -> Option<&Block> {
        self.block_index
            .get(&line_number)
            .map(|index| &self.blocks[*index])
    }

    fn build_program(mut blocks: Vec<Block>) -> Result<Program, Error> {
        let mut program_blocks = Vec::new();
        let mut block_index = HashMap::new();
        let mut datum = Vec::new();
        let mut index = 0;
        for block in blocks.drain(..) {
            match block {
                Block::Line {
                    line_number,
                    statement,
                    statement_source,
                } => match statement {
                    Statement::Data(mut statement_datum) => {
                        datum.append(&mut statement_datum);
                    }
                    _ => {
                        if block_index.insert(line_number, index).is_some() {
                            return Err(Error::DuplicateLineNumber { line_number });
                        }
                        index += 1;
                        program_blocks.push(Block::Line {
                            line_number,
                            statement,
                            statement_source,
                        })
                    }
                },
            }
        }
        Ok(Program {
            blocks: program_blocks,
            block_index,
            datum,
        })
    }
}

#[derive(Debug)]
pub enum Block<'a> {
    Line {
        line_number: u16,
        statement_source: Span<'a>,
        statement: Statement,
    },
}

#[derive(Debug, PartialEq)]
pub enum RelationalExpression {
    NumericComparison(NumericExpression, Relation, NumericExpression),
    StringComparison(StringExpression, EqualityRelation, StringExpression),
}

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub control_variable: NumericVariable,
    pub initial_value: NumericExpression,
    pub limit: NumericExpression,
    pub increment: NumericExpression,
}

#[derive(Debug, PartialEq)]
pub struct OnGotoStatement {
    pub numeric_expression: NumericExpression,
    pub line_numbers: Vec<u16>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Print(PrintStatement),
    Let(LetStatement),
    Goto(u16),
    Gosub(u16),
    IfThen(RelationalExpression, u16),
    OnGoto(OnGotoStatement),
    For(ForStatement),
    Next(NumericVariable),
    Read(Vec<Variable>),
    Data(Vec<Expression>),
    Restore,
    Rem,
    Return,
    Stop,
    End,
}

// 6. Constants

#[derive(Debug)]
pub enum Constant {
    Numeric {
        sign: Sign,
        significand: f64,
        exrad: i32,
    },
    String(StringConstant),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign {
    Pos,
    Neg,
}

impl ops::Mul<f64> for Sign {
    type Output = f64;
    fn mul(self, val: f64) -> Self::Output {
        match self {
            Sign::Pos => val,
            Sign::Neg => -val,
        }
    }
}

impl ops::Mul<i32> for Sign {
    type Output = i32;
    fn mul(self, val: i32) -> Self::Output {
        match self {
            Sign::Pos => val,
            Sign::Neg => -val,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringConstant(pub String);

// 7. Variable
// TODO: numeric-array-element is missing

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum NumericVariable {
    Simple { letter: char, digit: Option<u8> },
}

impl fmt::Display for NumericVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumericVariable::Simple { letter, digit } => {
                f.write_char(*letter)?;
                if let Some(digit) = digit {
                    write!(f, "{}", digit)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StringVariable(pub char);

impl fmt::Display for StringVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}$", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub enum Variable {
    Numeric(NumericVariable),
    String(StringVariable),
}

// 8. Expressions

#[derive(Debug, PartialEq)]
pub enum Expression {
    Numeric(NumericExpression),
    String(StringExpression),
}

#[derive(Debug, PartialEq)]
pub struct NumericExpression {
    pub terms: Vec<(Sign, Term)>,
}

impl NumericExpression {
    pub fn new(sign: Option<Sign>, term: Term, mut terms: Vec<(Sign, Term)>) -> Self {
        let mut all_terms = vec![(sign.unwrap_or(Sign::Pos), term)];
        all_terms.append(&mut terms);
        Self { terms: all_terms }
    }

    pub fn with_constant(value: f64) -> Self {
        NumericExpression::new(
            Some(if value >= 0.0 { Sign::Pos } else { Sign::Neg }),
            Term::new(
                Factor::new(Primary::Constant(value.abs(), 0), vec![]),
                vec![],
            ),
            vec![],
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Term {
    pub factor: Factor,
    pub factors: Vec<(Multiplier, Factor)>,
}

impl Term {
    pub fn new(factor: Factor, factors: Vec<(Multiplier, Factor)>) -> Self {
        Self { factor, factors }
    }
}

#[derive(Debug, PartialEq)]
pub struct Factor {
    pub primaries: Vec<Primary>,
}

impl Factor {
    pub fn new(primary: Primary, mut factors: Vec<Primary>) -> Self {
        let mut primaries = vec![primary];
        primaries.append(&mut factors);
        Self { primaries }
    }
}

#[derive(Debug, PartialEq)]
pub enum Multiplier {
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Primary {
    Variable(NumericVariable),
    Constant(f64, i32),
    Expression(NumericExpression),
}

#[derive(Debug, PartialEq)]
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

// 11. LET statement

#[derive(Debug, PartialEq)]
pub enum LetStatement {
    Numeric {
        variable: NumericVariable,
        expression: NumericExpression,
    },
    String {
        variable: StringVariable,
        expression: StringExpression,
    },
}

// 12. Control statements

#[derive(Debug, PartialEq, Eq)]
pub enum Relation {
    LessThan,
    LessThanOrEqualTo,
    EqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    NotEqualTo,
}

#[derive(Debug, PartialEq, Eq)]
pub enum EqualityRelation {
    EqualTo,
    NotEqualTo,
}

// 14. PRINT statement

#[derive(Debug, PartialEq)]
pub struct PrintStatement {
    pub list: Vec<PrintItem>,
}

#[derive(Debug, PartialEq)]
pub enum PrintItem {
    Expression(Expression),
    TabCall(NumericExpression),
    Comma,
    Semicolon,
}

pub fn new_print_items(
    items: Vec<(Option<PrintItem>, PrintItem)>,
    trailing_item: Option<PrintItem>,
) -> Vec<PrintItem> {
    let mut res = Vec::new();
    for (item, sep) in items {
        if let Some(item) = item {
            res.push(item);
        }
        res.push(sep);
    }
    if let Some(item) = trailing_item {
        res.push(item);
    }
    res
}
