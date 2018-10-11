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
    pub data: Vec<Datum>,
    pub array_dims: HashMap<char, ArrayDimension>,
    pub array_values_len: usize,
    pub array_base: OptionBase,
    pub fn_translation_table: [u16; 26],
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
                let mut program = Program {
                    fn_translation_table: [FIRST_INTERNAL_LINE_NUMBER; 26],
                    ..Program::default()
                };

                let mut builder_state = ProgramBuilderState::default();
                program.build(blocks, &mut builder_state)?;
                program.validate(&builder_state, source_code)?;
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
        state: &mut ProgramBuilderState,
    ) -> Result<(), Error> {
        // 1. Create index of all blocks.
        // 2. Move out all data block into `data`.
        // 3. Flatten FOR blocks and generate equivalent code without FOR.
        // 4. Collect all array dimensions.
        for block in blocks {
            match block {
                Block::Line {
                    line_number,
                    statement_source,
                    mut statement,
                    ..
                } => {
                    let mut replace_by_rem = false;

                    match statement {
                        Statement::Data(ref mut statement_data) => {
                            self.data.append(statement_data);
                            replace_by_rem = true;
                        }
                        Statement::Dim(ref ar_decls) => {
                            for decl in ar_decls {
                                let dim = ArrayDimension {
                                    dim1: decl.bounds.0 as usize + 1,
                                    dim2: decl.bounds.1.map(|d| d as usize + 1),
                                    offset: self.array_values_len,
                                };
                                let dim_len = dim.len();
                                let inserted = self.array_dims.insert(decl.letter, dim);
                                self.array_values_len += dim_len;
                                if inserted.is_some() {
                                    return Err(Error::Redimensioned {
                                        src_line_number: line_number,
                                        variable: decl.letter,
                                        bounds: decl.bounds,
                                    });
                                }
                            }
                            replace_by_rem = true;
                        }
                        Statement::Let(LetStatement::Numeric {
                            variable:
                                NumericVariable::Array(ArrayVariable {
                                    letter,
                                    ref subscript,
                                }),
                            ..
                        }) => {
                            let offset = self.array_values_len;
                            let mut offset_inc = 0;
                            let dim = self.array_dims.entry(letter).or_insert_with(|| {
                                let dim = ArrayDimension {
                                    dim1: 11,
                                    dim2: subscript.1.as_ref().map(|_| 11),
                                    offset,
                                };
                                offset_inc += dim.len();
                                dim
                            });
                            if subscript.1.is_some() != dim.dim2.is_some() {
                                let info = if dim.dim2.is_some() {
                                    "it was previously used or DIM as a two-dimension array"
                                } else {
                                    "it was previously used or DIM as a one-dimension array"
                                };

                                return Err(Error::TypeMismatch {
                                    src_line_number: line_number,
                                    variable: format!("{}", letter),
                                    info: info.into(),
                                });
                            }
                            self.array_values_len += offset_inc;
                        }
                        Statement::Let(LetStatement::Numeric {
                            variable:
                                NumericVariable::Simple(SimpleNumericVariable {
                                    letter,
                                    digit: None,
                                }),
                            ..
                        }) => {
                            if let Some(dim) = self.array_dims.get(&letter) {
                                let info = if dim.dim2.is_some() {
                                    "it was previously used or DIM as a two-dimension array"
                                } else {
                                    "it was previously used or DIM as a one-dimension array"
                                };
                                return Err(Error::TypeMismatch {
                                    src_line_number: line_number,
                                    variable: format!("{}", letter),
                                    info: info.into(),
                                });
                            }
                        }
                        Statement::OptionBase(base) => {
                            if !self.array_dims.is_empty() {
                                return Err(Error::InvalidOptionBase {
                                    src_line_number: line_number,
                                    base: base as usize,
                                });
                            }

                            self.array_base = base;
                            replace_by_rem = true;
                        }
                        Statement::Def(DefFunction { name, .. }) => {
                            self.fn_translation_table[Self::letter_address(name)] = line_number;
                        }
                        _ => (),
                    };

                    if replace_by_rem {
                        // TODO: Remove this and other REMs. For that, we need to check that there
                        // are not jumps on such lines, and if there are some, replace them.
                        self.blocks.push(Block::Line {
                            line_number,
                            statement_source,
                            statement: Statement::Rem,
                        })
                    } else {
                        self.blocks.push(Block::Line {
                            line_number,
                            statement_source,
                            statement,
                        })
                    }

                    state.index_for_level(line_number);
                    self.index_last_block()?;
                }
                // Flatten FOR...NEXT by replacing FOR statement with LET preamble and
                // IF statement, NEXT statement with GOSUB.
                Block::For {
                    for_line,
                    blocks,
                    next_line,
                } => {
                    let control_variable = for_line.for_statement.control_variable;
                    // check that control variables matches in FOR and NEXT
                    if control_variable != next_line.next_statement.control_variable {
                        return Err(Error::InvalidControlVariable {
                            src_line_number: next_line.line_number,
                            control_variable: format!(
                                "{}",
                                next_line.next_statement.control_variable
                            ),
                        });
                    }
                    // check that control variable is not used in a parent FOR..NEXT
                    if let Some(outer_line_number) = state
                        .for_control_variables_stack
                        .get(&control_variable)
                        .cloned()
                    {
                        return Err(Error::ControlVariableReuse {
                            src_line_number: for_line.line_number,
                            outer_line_number,
                        });
                    }
                    state
                        .for_control_variables_stack
                        .insert(control_variable, for_line.line_number);

                    // LET .. = limit
                    let limit = LimitVariable {
                        line_number: for_line.line_number,
                    };
                    self.blocks.push(Block::Line {
                        line_number: for_line.line_number,
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: NumericVariable::Limit(limit),
                            expression: for_line.for_statement.limit,
                        }),
                    });
                    state.index_for_level(for_line.line_number);
                    self.index_last_block()?;

                    // LET .. = increment
                    let increment = IncrementVariable {
                        line_number: for_line.line_number,
                    };
                    self.blocks.push(Block::Line {
                        line_number: state.next_internal_line_number(),
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: NumericVariable::Increment(increment),
                            expression: for_line
                                .for_statement
                                .increment
                                .unwrap_or(NumericExpression::with_constant(1.0)),
                        }),
                    });
                    self.index_last_block()?;

                    // LET control_varible = initial_value
                    self.blocks.push(Block::Line {
                        line_number: state.next_internal_line_number(),
                        statement_source: for_line.statement_source,
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: NumericVariable::Simple(control_variable),
                            expression: for_line.for_statement.initial_value,
                        }),
                    });
                    self.index_last_block()?;

                    // IF (v - limit) * SGN( increment ) > 0 THEN [line after NEXT]
                    let diff = NumericExpression {
                        terms: vec![
                            (
                                Sign::Pos,
                                Term::with_variable(NumericVariable::Simple(control_variable)),
                            ),
                            (
                                Sign::Neg,
                                Term::with_variable(NumericVariable::Limit(limit)),
                            ),
                        ],
                    };
                    let inc = NumericExpression {
                        terms: vec![(
                            Sign::Pos,
                            Term::with_variable(NumericVariable::Increment(increment)),
                        )],
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

                    let continue_line_number = state.next_internal_line_number();
                    let break_line_number = state.next_internal_line_number();
                    self.blocks.push(Block::Line {
                        line_number: continue_line_number,
                        statement_source: for_line.statement_source,
                        statement: Statement::IfThen(
                            RelationalExpression::NumericComparison(
                                condition,
                                Relation::GreaterThan,
                                NumericExpression::with_constant(0.0),
                            ),
                            break_line_number,
                        ),
                    });
                    self.index_last_block()?;

                    // add inner blocks recursively
                    self.build(blocks, state)?;

                    state.for_control_variables_stack.remove(&control_variable);

                    // LET control_variable = control_variable + increment
                    self.blocks.push(Block::Line {
                        line_number: next_line.line_number,
                        statement_source: next_line.statement_source,
                        statement: Statement::Let(LetStatement::Numeric {
                            variable: NumericVariable::Simple(control_variable),
                            expression: NumericExpression {
                                terms: vec![
                                    (
                                        Sign::Pos,
                                        Term::with_variable(NumericVariable::Simple(
                                            control_variable,
                                        )),
                                    ),
                                    (
                                        Sign::Pos,
                                        Term::with_variable(NumericVariable::Increment(increment)),
                                    ),
                                ],
                            },
                        }),
                    });
                    state.index_for_level(next_line.line_number);
                    self.index_last_block()?;

                    // GOTO continue_line_number
                    self.blocks.push(Block::Line {
                        line_number: state.next_internal_line_number(),
                        statement_source: next_line.statement_source,
                        statement: Statement::Goto(continue_line_number),
                    });
                    self.index_last_block()?;

                    // REM "continue after FOR block"
                    self.blocks.push(Block::Line {
                        line_number: break_line_number,
                        statement_source: Span::new(CompleteStr("")),
                        statement: Statement::Rem,
                    });
                    self.index_last_block()?;
                }
            }
        }
        Ok(())
    }

    /// Validation that can be only made after the whole program was built.
    ///
    /// Validations:
    ///
    /// * Check for valid line numbers in statements refering such.
    /// * Check for jumping into a FOR block
    fn validate(
        &self,
        builder_state: &ProgramBuilderState,
        source_code: &str,
    ) -> Result<(), Error> {
        // check for defined line number
        let check_line_number = |ref_line_number, line_number, statement_source: &Span| {
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

        for block in &self.blocks {
            match block {
                Block::Line {
                    line_number,
                    statement,
                    statement_source,
                } => match statement {
                    Statement::Goto(ref_line_number) => {
                        check_line_number(*ref_line_number, *line_number, statement_source)?;

                        // check jumps into FOR (for non-generated jumps)
                        // Note: jumping out of a FOR..NEXT block is fine
                        if *line_number < FIRST_INTERNAL_LINE_NUMBER
                            && *ref_line_number < FIRST_INTERNAL_LINE_NUMBER
                        {
                            let current_level = builder_state.for_levels.get(line_number);
                            let ref_level = builder_state.for_levels.get(ref_line_number);
                            if current_level < ref_level {
                                return Err(Error::JumpIntoFor {
                                    src_line_number: *line_number,
                                });
                            }
                        }
                    }
                    Statement::IfThen(_, ref_line_number) | Statement::Gosub(ref_line_number) => {
                        check_line_number(*ref_line_number, *line_number, statement_source)?;
                    }
                    Statement::OnGoto(OnGotoStatement { line_numbers, .. }) => {
                        for ref_line_number in line_numbers {
                            check_line_number(*ref_line_number, *line_number, statement_source)?;
                        }
                    }
                    Statement::Def(DefFunction {
                        name, expression, ..
                    }) => {
                        let mut fns = Vec::new();
                        expression.function_calls(&mut fns);
                        if fns.iter().position(|f| f == name).is_some() {
                            let statement_source = source_code[statement_source.offset..]
                                .lines()
                                .next()
                                .unwrap()
                                .into();
                            return Err(Error::UndefinedFunction {
                                src_line_number: *line_number,
                                name: *name,
                                statement_source,
                            });
                        }
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        Ok(())
    }

    pub fn letter_address(letter: char) -> usize {
        match letter {
            'A'..='Z' => letter as usize - 'A' as usize,
            _ => panic!("invalid letter"),
        }
    }
}

#[derive(Debug)]
struct ProgramBuilderState {
    internal_line_numer: u16,
    for_control_variables_stack: HashMap<SimpleNumericVariable, u16 /* line number */>,
    /// FOR deepness level for each line
    for_levels: HashMap<u16 /* line number */, usize>,
}

impl Default for ProgramBuilderState {
    fn default() -> Self {
        Self {
            internal_line_numer: FIRST_INTERNAL_LINE_NUMBER,
            for_control_variables_stack: HashMap::new(),
            for_levels: HashMap::new(),
        }
    }
}

impl ProgramBuilderState {
    fn next_internal_line_number(&mut self) -> u16 {
        let next = self.internal_line_numer;
        self.internal_line_numer += 1;
        next
    }

    fn index_for_level(&mut self, line_number: u16) {
        self.for_levels
            .insert(line_number, self.for_control_variables_stack.len());
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
    Def(DefFunction),
    Let(LetStatement),
    Goto(u16),
    Gosub(u16),
    OnGoto(OnGotoStatement),
    IfThen(RelationalExpression, u16),
    Input(Vec<Variable>),
    Read(Vec<Variable>),
    Data(Vec<Datum>),
    Restore,
    Rem,
    Return,
    Stop,
    End,
    Dim(Vec<ArrayDeclaration>),
    OptionBase(OptionBase),
}

#[derive(Debug, PartialEq)]
pub struct ForStatement {
    pub control_variable: SimpleNumericVariable,
    pub initial_value: NumericExpression,
    pub limit: NumericExpression,
    pub increment: Option<NumericExpression>,
}

#[derive(Debug, PartialEq)]
pub struct NextStatement {
    pub control_variable: SimpleNumericVariable,
}

#[derive(Debug, PartialEq)]
pub struct OnGotoStatement {
    pub numeric_expression: NumericExpression,
    pub line_numbers: Vec<u16>,
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

/// Numeric variable containing expressions.
///
/// It cannot be stored inside program memory, due to possible deeply nested
/// expressions in array variables.
#[derive(Debug, PartialEq)]
pub enum NumericVariable {
    Simple(SimpleNumericVariable),
    Limit(LimitVariable),
    Increment(IncrementVariable),
    Array(ArrayVariable),
}

impl fmt::Display for NumericVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumericVariable::Simple(v) => write!(f, "{}", v),
            NumericVariable::Array(_) => panic!("logic error: formatting array variable"),
            NumericVariable::Limit(_) | NumericVariable::Increment(_) => {
                panic!("logic error: formatting internal variable")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PlainNumericVariable {
    Simple(SimpleNumericVariable),
    Limit(LimitVariable),
    Increment(IncrementVariable),
    Array(PlainArrayVariable),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct LimitVariable {
    pub line_number: u16,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct IncrementVariable {
    pub line_number: u16,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SimpleNumericVariable {
    pub letter: char,
    pub digit: Option<u8>,
}

impl fmt::Display for SimpleNumericVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.letter)?;
        if let Some(digit) = self.digit {
            write!(f, "{}", digit)?;
        }
        Ok(())
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

#[derive(Debug, PartialEq)]
pub struct ArrayVariable {
    pub letter: char,
    pub subscript: (NumericExpression, Option<NumericExpression>),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PlainArrayVariable {
    pub letter: char,
    pub subscript: (usize, Option<usize>),
}

impl fmt::Display for PlainArrayVariable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (subscript1, subscript2) = self.subscript;
        if let Some(subscript2) = subscript2 {
            write!(f, "{}({},{})", self.letter, subscript1, subscript2)
        } else {
            write!(f, "{}({})", self.letter, subscript1)
        }
    }
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

    pub fn function_calls(&self, result: &mut Vec<char>) {
        for (_, t) in &self.terms {
            t.function_calls(result);
        }
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

    pub fn function_calls(&self, result: &mut Vec<char>) {
        self.factor.function_calls(result);
        for (_, f) in &self.factors {
            f.function_calls(result);
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

    pub fn function_calls(&self, result: &mut Vec<char>) {
        for p in &self.primaries {
            p.function_calls(result);
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
    DefFunctionCall(DefFunctionCall),
    Expression(NumericExpression),
}

impl Primary {
    pub fn function_calls(&self, result: &mut Vec<char>) {
        match self {
            Primary::DefFunctionCall(DefFunctionCall { name, .. }) => result.push(*name),
            Primary::Expression(e) => e.function_calls(result),
            _ => (),
        }
    }
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

// 10. User defined functions

#[derive(Debug, PartialEq)]
pub struct DefFunction {
    pub name: char,
    pub parameter: Option<SimpleNumericVariable>,
    pub expression: NumericExpression,
}

#[derive(Debug, PartialEq)]
pub struct DefFunctionCall {
    pub name: char,
    pub arg: Option<NumericExpression>,
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

// 17. DATA statement

#[derive(Debug, PartialEq, Eq)]
pub enum Datum {
    Quoted(StringConstant),
    Unquoted(StringConstant),
}

impl AsRef<str> for Datum {
    fn as_ref(&self) -> &str {
        match self {
            Datum::Quoted(s) | Datum::Unquoted(s) => &s.0,
        }
    }
}

// 18. ARRAY declarations

#[derive(Debug, PartialEq)]
pub struct ArrayDeclaration {
    pub letter: char,
    pub bounds: (u64, Option<u64>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptionBase {
    Base0 = 0,
    Base1 = 1,
}

impl Default for OptionBase {
    fn default() -> Self {
        OptionBase::Base0
    }
}

#[derive(Debug)]
pub struct ArrayDimension {
    pub dim1: usize,
    pub dim2: Option<usize>,
    pub offset: usize,
}

impl ArrayDimension {
    pub fn len(&self) -> usize {
        self.dim1 * self.dim2.unwrap_or(1)
    }
}
