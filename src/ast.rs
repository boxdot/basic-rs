use error::Error;
use parser::Span;

use nom::types::CompleteStr;

use std::collections::HashMap;
use std::fmt::{self, Write};
use std::ops;

// Note: a valid line number can only have max 4 digits.
const FIRST_INTERNAL_LINE_NUMBER: u16 = 10001;

#[derive(Debug, Default)]
pub struct Program<'a> {
    pub blocks: Vec<Block<'a>>,
    block_index: HashMap<u16, usize>,
    pub data: Vec<Constant>,
}

impl<'a> Program<'a> {
    pub fn new<'b>(blocks: Vec<Block<'a>>, source_code: &'b str) -> Result<Program<'a>, Error> {
        let end_statement_pos = blocks.iter().position(|b| match b {
            Block::Line {
                statement: Statement::End,
                ..
            } => true,
            _ => false,
        });

        if let Some(end_statement_pos) = end_statement_pos {
            if end_statement_pos + 1 != blocks.len() {
                let line_numbers = blocks
                    .iter()
                    .skip(end_statement_pos + 1)
                    .map(Block::line_number);
                Err(Error::StatementsAfterEnd {
                    line_numbers: line_numbers.collect(),
                })
            } else {
                let mut program = Program::default();
                program.build(blocks, FIRST_INTERNAL_LINE_NUMBER)?;
                program.validate(source_code)?;
                Ok(program)
            }
        } else {
            Err(Error::MissingEnd {
                src_line_number: blocks.last().map(Block::line_number).unwrap_or(0),
            })
        }
    }

    pub fn first_block(&self) -> &Block {
        self.blocks.first().expect("logic error")
    }

    pub fn next_block<'b>(&'a self, block: &'b Block) -> &'a Block {
        let line_number = block.line_number();
        let index = self.block_index.get(&line_number).expect("logic error");
        &self.blocks[index + 1]
    }

    pub fn get_block_by_line_number(&self, line_number: u16) -> Option<&Block> {
        self.block_index
            .get(&line_number)
            .map(|index| &self.blocks[*index])
    }

    // TODO: Refactor to use a block_pointer and remove this function
    fn index_last_block(&mut self) -> Result<(), Error> {
        let line_number = self.blocks.last().unwrap().line_number();
        let index = self.blocks.len() - 1;
        if self.block_index.insert(line_number, index).is_some() {
            Err(Error::DuplicateLineNumber { line_number })
        } else {
            Ok(())
        }
    }

    fn build(
        &mut self,
        blocks: Vec<Block<'a>>,
        mut internal_line_numer: u16,
    ) -> Result<u16, Error> {
        // 1. Create index of all blocks.
        // 2. Move out all data block into `data`
        // FIXME: We could increase performance by removing data blocks from all blocks,
        // however we still need to keep index of them, since a GOTO statement could jump
        // on a data block. Instead, we replace them with REM.
        // 3. Flatten FOR blocks and generate equivalent code without FOR.
        for block in blocks {
            match block {
                Block::Line {
                    line_number,
                    statement_source,
                    statement,
                    ..
                } => {
                    match statement {
                        Statement::Data(mut statement_data) => {
                            self.data.append(&mut statement_data);
                            self.blocks.push(Block::Line {
                                line_number,
                                statement_source,
                                statement: Statement::Rem,
                            })
                        }
                        other_statement => self.blocks.push(Block::Line {
                            line_number,
                            statement_source,
                            statement: other_statement,
                        }),
                    };
                    self.index_last_block()?;
                }
                // Flatten FOR...NEXT by replacing FOR statement with LET preamble and
                // IF statement, NEXT statement with GOSUB.
                Block::For {
                    for_line,
                    blocks,
                    next_line,
                } => {
                    // check that control variables match
                    if for_line.for_statement.control_variable
                        != next_line.next_statement.control_variable
                    {
                        return Err(Error::InvalidControlVariable {
                            src_line_number: for_line.line_number,
                            control_variable: format!(
                                "{}",
                                for_line.for_statement.control_variable
                            ),
                        });
                    }

                    // LET .. = limit
                    let limit = NumericVariable::Limit {
                        line_number: for_line.line_number,
                    };
                    self.blocks.push(Block::Line {
                        line_number: internal_line_numer,
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: limit,
                            expression: for_line.for_statement.limit,
                        }),
                    });
                    internal_line_numer += 1;
                    self.index_last_block()?;

                    // LET .. = increment
                    let increment = NumericVariable::Increment {
                        line_number: for_line.line_number,
                    };
                    self.blocks.push(Block::Line {
                        line_number: internal_line_numer,
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: increment,
                            expression: for_line
                                .for_statement
                                .increment
                                .unwrap_or(NumericExpression::with_constant(1.0)),
                        }),
                    });
                    internal_line_numer += 1;
                    self.index_last_block()?;

                    // LET control_varible = initial_value
                    self.blocks.push(Block::Line {
                        line_number: internal_line_numer,
                        statement_source: for_line.statement_source,
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: for_line.for_statement.control_variable,
                            expression: for_line.for_statement.initial_value,
                        }),
                    });
                    internal_line_numer += 1;
                    self.index_last_block()?;

                    // IF (v - limit) * SGN( increment ) > 0 THEN [line after NEXT]
                    let diff = NumericExpression {
                        terms: vec![
                            (
                                Sign::Pos,
                                Term::with_variable(for_line.for_statement.control_variable),
                            ),
                            (Sign::Neg, Term::with_variable(limit)),
                        ],
                    };
                    let inc = NumericExpression {
                        terms: vec![(Sign::Pos, Term::with_variable(increment))],
                    };
                    let sgn_inc = Function::Sgn(inc);
                    let condition = NumericExpression {
                        terms: vec![(
                            Sign::Pos,
                            Term {
                                factor: Factor::with_expression(diff),
                                factors: vec![(Multiplier::Mul, Factor::with_function(sgn_inc))],
                            },
                        )],
                    };

                    let after_next_line_number = internal_line_numer;
                    internal_line_numer += 1;
                    self.blocks.push(Block::Line {
                        line_number: for_line.line_number,
                        statement_source: for_line.statement_source,
                        statement: Statement::IfThen(
                            RelationalExpression::NumericComparison(
                                condition,
                                Relation::GreaterThan,
                                NumericExpression::with_constant(0.0),
                            ),
                            after_next_line_number,
                        ),
                    });
                    self.index_last_block()?;

                    // add inner blocks recursively
                    internal_line_numer = self.build(blocks, internal_line_numer)?;

                    // LET control_variable = control_variable + increment
                    self.blocks.push(Block::Line {
                        line_number: next_line.line_number,
                        statement_source: for_line.statement_source,
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: for_line.for_statement.control_variable,
                            expression: NumericExpression {
                                terms: vec![
                                    (
                                        Sign::Pos,
                                        Term::with_variable(
                                            for_line.for_statement.control_variable,
                                        ),
                                    ),
                                    (Sign::Pos, Term::with_variable(increment)),
                                ],
                            },
                        }),
                    });
                    self.index_last_block()?;

                    // GOTO [FOR line number]
                    self.blocks.push(Block::Line {
                        line_number: internal_line_numer,
                        statement_source: next_line.statement_source,
                        statement: Statement::Goto(for_line.line_number),
                    });
                    internal_line_numer += 1;
                    self.index_last_block()?;

                    // REM "continue after FOR block"
                    self.blocks.push(Block::Line {
                        line_number: after_next_line_number,
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Rem,
                    });
                    self.index_last_block()?;
                }
            }
        }
        Ok(internal_line_numer)
    }

    /// Validation that can be only made after the whole program was built.
    ///
    /// Validations:
    ///
    /// * Check for valid line numbers in statements refering such.
    fn validate(&self, source_code: &str) -> Result<(), Error> {
        let check_valid_line_number = |ref_line_number, line_number, statement_source: &Span| {
            if self.block_index.get(&ref_line_number).is_none() {
                Err(Error::UndefinedLineNumber {
                    src_line_number: line_number,
                    line_number: ref_line_number,
                    statement_source: source_code[statement_source.offset..]
                        .lines()
                        .next()
                        .unwrap()
                        .into(),
                })
            } else {
                Ok(())
            }
        };

        // check for invalid line label
        for block in &self.blocks {
            match block {
                Block::Line {
                    line_number,
                    statement,
                    statement_source,
                } => match statement {
                    Statement::Goto(ref_line_number) => {
                        check_valid_line_number(*ref_line_number, *line_number, statement_source)?;
                    }
                    Statement::IfThen(_, ref_line_number) => {
                        check_valid_line_number(*ref_line_number, *line_number, statement_source)?;
                    }
                    Statement::Gosub(ref_line_number) => {
                        check_valid_line_number(*ref_line_number, *line_number, statement_source)?;
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Block<'a> {
    Line {
        line_number: u16,
        statement_source: Span<'a>,
        statement: Statement,
    },
    For {
        for_line: ForLine<'a>,
        blocks: Vec<Block<'a>>,
        next_line: NextLine<'a>,
    },
}

impl<'a> Block<'a> {
    fn line_number(&self) -> u16 {
        match self {
            Block::Line { line_number, .. } => *line_number,
            Block::For { for_line, .. } => for_line.line_number,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ForLine<'a> {
    pub line_number: u16,
    pub statement_source: Span<'a>,
    pub for_statement: ForStatement,
}

#[derive(Debug, PartialEq)]
pub struct NextLine<'a> {
    pub line_number: u16,
    pub statement_source: Span<'a>,
    pub next_statement: NextStatement,
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

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub control_variable: NumericVariable,
    pub initial_value: NumericExpression,
    pub limit: NumericExpression,
    pub increment: Option<NumericExpression>,
}

#[derive(Debug, PartialEq)]
pub struct NextStatement {
    pub control_variable: NumericVariable,
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
    Limit { line_number: u16 },
    Increment { line_number: u16 },
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
            _ => panic!("logic error: formatting internal variable"),
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

    fn with_variable(variable: NumericVariable) -> Self {
        Self {
            factor: Factor::with_variable(variable),
            factors: Vec::new(),
        }
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

    fn with_variable(variable: NumericVariable) -> Self {
        Self {
            primaries: vec![Primary::Variable(variable)],
        }
    }

    fn with_expression(expression: NumericExpression) -> Self {
        Self {
            primaries: vec![Primary::Expression(expression)],
        }
    }

    fn with_function(f: Function) -> Self {
        Self {
            primaries: vec![Primary::Function(f)],
        }
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
    Function(Function),
    Expression(NumericExpression),
}

#[derive(Debug, PartialEq)]
pub enum StringExpression {
    Variable(StringVariable),
    Constant(StringConstant),
}

// 9. Implementation supplied functions

#[derive(Debug, PartialEq)]
pub enum Function {
    Abs(NumericExpression),
    Atn(NumericExpression),
    Cos(NumericExpression),
    Exp(NumericExpression),
    Int(NumericExpression),
    Log(NumericExpression),
    Rnd,
    Sgn(NumericExpression),
    Sin(NumericExpression),
    Sqr(NumericExpression),
    Tan(NumericExpression),
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
