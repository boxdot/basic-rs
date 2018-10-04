use error::Error;
use parser::Span;

use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops;

#[derive(Debug)]
pub struct Program<'a> {
    pub blocks: Vec<Block<'a>>,
    block_index: HashMap<u16, usize>,
    pub data: Vec<Constant>,
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
                Self::build_program(blocks)
            }
        } else {
            Err(Error::MissingEnd {
                src_line_number: blocks
                    .last()
                    .map(|b| match b {
                        Block::Line { line_number, .. } => *line_number,
                    }).unwrap_or(0),
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
        let mut block_index = HashMap::new();
        let mut data = Vec::new();
        // Create index of all blocks and move out all data block into `data`
        // FIXME: We could increase performance by removing data blocks from all blocks,
        // however we still need to keep index of them, since a GOTO statement could jump
        // on a data block. For that, we most likely need to use something like skip lists.
        for (index, block) in blocks.iter_mut().enumerate() {
            match block {
                Block::Line {
                    line_number,
                    ref mut statement,
                    ..
                } => {
                    match statement {
                        Statement::Data(ref mut statement_data) => {
                            data.append(statement_data);
                        }
                        _ => (),
                    };
                    if block_index.insert(*line_number, index).is_some() {
                        return Err(Error::DuplicateLineNumber {
                            line_number: *line_number,
                        });
                    }
                }
            }
        }
        Ok(Program {
            blocks,
            block_index,
            data,
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
pub enum Statement {
    Print(PrintStatement),
    Let(LetStatement),
    Goto(u16),
    Gosub(u16),
    IfThen(RelationalExpression, u16),
    Read(Vec<Variable>),
    Data(Vec<Constant>),
    Restore,
    Rem,
    Return,
    Stop,
    End,
}

// 6. Constants

#[derive(Debug, PartialEq)]
pub enum Constant {
    Numeric(NumericConstant),
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

#[derive(Debug, PartialEq)]
pub struct NumericConstant {
    pub sign: Sign,
    pub significand: f64,
    pub exrad: i32,
}

impl From<(f64, i32)> for NumericConstant {
    fn from((significand, exrad): (f64, i32)) -> Self {
        Self {
            sign: if significand >= 0.0 {
                Sign::Pos
            } else {
                Sign::Neg
            },
            significand: significand.abs(),
            exrad,
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

    #[cfg(test)]
    pub fn with_constant(value: f64) -> Self {
        NumericExpression::new(
            Some(if value >= 0.0 { Sign::Pos } else { Sign::Neg }),
            Term::new(
                Factor::new(
                    Primary::Constant(NumericConstant::from((value.abs(), 0))),
                    vec![],
                ),
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
    Constant(NumericConstant),
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
