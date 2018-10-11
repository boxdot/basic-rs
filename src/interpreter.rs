use ast::*;
use error::Error;
use format::format_float;
use parser;

use itertools::Itertools;
use nom::types::CompleteStr;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

use std::collections::HashMap;
use std::io::Write;

pub struct Interpreter<'a> {
    program: &'a Program<'a>,
    source_code: &'a str,
    state: State,
}

#[derive(Debug)]
struct State {
    /// BASIC line number label of the current statement
    current_line_number: u16,
    /// Offset in the source code of the current statement
    current_source_offset: usize,
    /// current columnar position of the cursor
    columnar_position: usize,
    /// values of numeric variables
    numeric_values: HashMap<PlainNumericVariable, f64>,
    /// values of string variables
    string_values: HashMap<StringVariable, String>,
    /// values of array variables
    array_values: Vec<f64>,
    /// stack of return line numbers for routines
    stack: Vec<u16>,
    /// DATA statement pointer
    data_pointer: u16,
    /// random number generator
    rng: SmallRng,
}

impl Default for State {
    fn default() -> Self {
        Self {
            current_line_number: Default::default(),
            current_source_offset: Default::default(),
            columnar_position: Default::default(),
            numeric_values: Default::default(),
            string_values: Default::default(),
            array_values: Default::default(),
            stack: Default::default(),
            data_pointer: Default::default(),
            rng: SmallRng::from_seed([
                0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF,
            ]),
        }
    }
}

impl State {
    fn new(array_values_len: usize) -> Self {
        Self {
            array_values: vec![0.0; array_values_len],
            rng: SmallRng::from_seed([
                0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF,
            ]),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
enum Action {
    NextLine,
    Return,
    Stop,
    Goto(u16),
    Gosub(u16),
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program, source_code: &'a str) -> Self {
        Self {
            program,
            source_code,
            state: State::new(program.array_values_len),
        }
    }

    pub fn evaluate<W: Write, V: Write>(
        mut self,
        stdout: &mut W,
        stderr: &mut V,
    ) -> Result<(), Error> {
        let mut block = self.program.first_block();
        loop {
            let action = match block {
                Block::Line {
                    line_number,
                    statement,
                    statement_source,
                } => {
                    self.state.current_line_number = *line_number;
                    self.state.current_source_offset = statement_source.offset;
                    self.evaluate_statement(statement, stdout, stderr)?
                }
                Block::For { .. } => {
                    panic!("logic error: FOR should be compiled to other statements")
                }
            };

            let src_line_number = self.state.current_line_number;
            let source_offset = self.state.current_source_offset;

            match action {
                Action::NextLine => block = self.program.next_block(block),
                Action::Goto(line_number) => {
                    block = self
                        .program
                        .get_block_by_line_number(line_number)
                        .ok_or_else(|| Error::UndefinedLineNumber {
                            src_line_number,
                            line_number,
                            statement_source: self.get_source_line(source_offset).into(),
                        })?;
                }
                Action::Gosub(line_number) => {
                    block = self
                        .program
                        .get_block_by_line_number(line_number)
                        .ok_or_else(|| Error::UndefinedLineNumber {
                            src_line_number,
                            line_number,
                            statement_source: self.get_source_line(source_offset).into(),
                        })?;
                    self.state.stack.push(self.state.current_line_number);
                }
                Action::Return => {
                    let prev_line_number = self
                        .state
                        .stack
                        .pop()
                        .ok_or_else(|| Error::StackUnderflow { src_line_number })?;
                    let prev_block = self
                        .program
                        .get_block_by_line_number(prev_line_number)
                        .ok_or_else(|| Error::UndefinedLineNumber {
                            src_line_number,
                            line_number: prev_line_number,
                            statement_source: self.get_source_line(source_offset).into(),
                        })?;
                    block = self.program.next_block(prev_block);
                }
                Action::Stop => break,
            }
        }

        Ok(())
    }

    fn evaluate_statement<W: Write, V: Write>(
        &mut self,
        statement: &Statement,
        stdout: &mut W,
        stderr: &mut V,
    ) -> Result<Action, Error> {
        let res = match statement {
            Statement::Print(statement) => {
                self.evaluate_print(statement, stdout, stderr)?;
                Action::NextLine
            }
            Statement::Let(statement) => {
                self.evaluate_let(statement, stderr)?;
                Action::NextLine
            }
            Statement::Goto(line_number) => Action::Goto(*line_number),
            Statement::Gosub(line_number) => Action::Gosub(*line_number),
            Statement::OnGoto(statement) => Action::Goto(self.evaluate_on_goto(statement, stderr)?),
            Statement::IfThen(if_statement, line_number) => {
                if self.evaluate_if(if_statement, stderr)? {
                    Action::Goto(*line_number)
                } else {
                    Action::NextLine
                }
            }
            Statement::Read(variables) => {
                self.evaluate_read(variables, stderr)?;
                Action::NextLine
            }
            Statement::Restore => {
                self.state.data_pointer = 0;
                Action::NextLine
            }
            Statement::Data(_) => Action::NextLine,
            Statement::Rem => Action::NextLine,
            Statement::Return => Action::Return,
            Statement::Stop => Action::Stop,
            Statement::End => Action::Stop,
            Statement::Dim(_) => Action::NextLine,
            Statement::OptionBase(_) => Action::NextLine,
        };
        Ok(res)
    }

    fn evaluate_print<W: Write, V: Write>(
        &mut self,
        statement: &PrintStatement,
        stdout: &mut W,
        stderr: &mut V,
    ) -> Result<(), Error> {
        const COLUMN_WIDTH: usize = 70;
        const NUM_PRINT_ZONES: usize = 5;
        const PRINT_ZONE_WIDTH: usize = COLUMN_WIDTH / NUM_PRINT_ZONES;

        let mut columnar_position = self.state.columnar_position;
        for item in &statement.list {
            match item {
                PrintItem::Expression(expression) => match expression {
                    Expression::String(string_expression) => {
                        let value = self.evaluate_string_expression(string_expression)?;
                        write!(stdout, "{}", value);
                        columnar_position += value.len();
                    }
                    Expression::Numeric(numeric_expression) => {
                        let value = self.evaluate_numeric_expression(numeric_expression, stderr)?;
                        let value_str = format_float(value);
                        write!(stdout, "{}", value_str);
                        columnar_position += value_str.len();
                    }
                },
                PrintItem::TabCall(numeric_expression) => {
                    let tab_width = self
                        .evaluate_numeric_expression(numeric_expression, stderr)?
                        .round() as i64;
                    if tab_width < 1 {
                        self.warn(stderr, format!("invalid TAB argument ({})", tab_width));
                        continue;
                    }

                    let tab_width = (tab_width as usize) % COLUMN_WIDTH;
                    if tab_width < columnar_position {
                        write!(stdout, "\n");
                        columnar_position = 0;
                    }

                    write!(stdout, "{:1$}", "", tab_width - columnar_position - 1);
                    columnar_position = tab_width - 1;
                }
                PrintItem::Comma => {
                    let current_print_zone = columnar_position / PRINT_ZONE_WIDTH;
                    if current_print_zone + 1 < NUM_PRINT_ZONES {
                        let next_columnar_position = (current_print_zone + 1) * PRINT_ZONE_WIDTH;
                        write!(
                            stdout,
                            "{:1$}",
                            "",
                            next_columnar_position - columnar_position
                        );
                        columnar_position = next_columnar_position;
                    } else {
                        write!(stdout, "\n");
                        columnar_position = 0;
                    }
                }
                PrintItem::Semicolon => (),
            }
        }
        self.state.columnar_position = columnar_position;

        let last_item_is_comma_or_semicolon = statement
            .list
            .last()
            .map(|s| match s {
                PrintItem::Semicolon => true,
                PrintItem::Comma => true,
                _ => false,
            }).unwrap_or(false);
        if !last_item_is_comma_or_semicolon {
            self.state.columnar_position = 0;
            write!(stdout, "\n");
        }

        Ok(())
    }

    fn evaluate_let<W: Write>(
        &mut self,
        statement: &LetStatement,
        stderr: &mut W,
    ) -> Result<(), Error> {
        match statement {
            LetStatement::Numeric {
                variable: NumericVariable::Array(ar),
                expression,
            } => {
                let (_, index) = self.make_array_plain(ar, stderr)?;
                let value = self.evaluate_numeric_expression(expression, stderr)?;
                let variable = &mut self.state.array_values[index];
                *variable = value;
            }
            LetStatement::Numeric {
                variable,
                expression,
            } => {
                let value = self.evaluate_numeric_expression(expression, stderr)?;
                let variable = self.make_plain(variable, stderr)?;
                self.state.numeric_values.insert(variable, value);
            }
            LetStatement::String {
                variable,
                expression,
            } => {
                let value = self.evaluate_string_expression(expression)?.into();
                self.state.string_values.insert(*variable, value);
            }
        }

        Ok(())
    }

    fn evaluate_if<W: Write>(
        &mut self,
        if_statement: &RelationalExpression,
        stderr: &mut W,
    ) -> Result<bool, Error> {
        match if_statement {
            RelationalExpression::StringComparison(
                left_string_expression,
                relation,
                right_string_expression,
            ) => {
                let left = self.evaluate_string_expression(left_string_expression)?;
                let right = self.evaluate_string_expression(right_string_expression)?;

                match relation {
                    EqualityRelation::EqualTo => Ok(left == right),
                    EqualityRelation::NotEqualTo => Ok(left != right),
                }
            }
            RelationalExpression::NumericComparison(
                left_numeric_expression,
                relation,
                right_numeric_expression,
            ) => {
                let left = self.evaluate_numeric_expression(left_numeric_expression, stderr)?;
                let right = self.evaluate_numeric_expression(right_numeric_expression, stderr)?;

                Ok(match relation {
                    Relation::LessThan => left < right,
                    Relation::LessThanOrEqualTo => left <= right,
                    Relation::EqualTo => left == right,
                    Relation::GreaterThanOrEqualTo => left >= right,
                    Relation::GreaterThan => left > right,
                    Relation::NotEqualTo => left != right,
                })
            }
        }
    }

    fn evaluate_string_expression(
        &'a self,
        expression: &'a StringExpression,
    ) -> Result<&'a str, Error> {
        match expression {
            StringExpression::Variable(variable) => {
                let value = self
                    .state
                    .string_values
                    .get(variable)
                    .map(|s| -> &str { s })
                    .unwrap_or("");
                Ok(value)
            }
            StringExpression::Constant(constant) => Ok(&constant.0),
        }
    }

    fn evaluate_numeric_expression<W: Write>(
        &mut self,
        expression: &NumericExpression,
        stderr: &mut W,
    ) -> Result<f64, Error> {
        expression
            .terms
            .iter()
            .map(|(sign, term)| self.evaluate_term(term, stderr).map(|term| (sign, term)))
            .fold_results(0.0, |acc, (sign, term)| acc + *sign * term)
    }

    fn evaluate_read<W: Write>(
        &mut self,
        variables: &Vec<Variable>,
        stderr: &mut W,
    ) -> Result<(), Error> {
        for variable in variables {
            let datum = self
                .program
                .data
                .get(self.state.data_pointer as usize)
                .ok_or_else(|| Error::InsufficientData {
                    src_line_number: self.state.current_line_number,
                })?;
            match variable {
                Variable::String(v) => {
                    self.state.string_values.insert(*v, datum.0.clone());
                }
                Variable::Numeric(v) => {
                    // FIXME: After reading unquoted string, we should try to parse it as
                    // numeric variable again, and store its value in datum.
                    let res = parser::numeric_constant(parser::Span::new(CompleteStr(&datum.0)));
                    match res {
                        Ok((remaining, ref c)) if remaining.fragment.is_empty() => {
                            let value = self.evaluate_numeric_constant(c, stderr)?;
                            let v = self.make_plain(v, stderr)?;
                            self.state.numeric_values.insert(v, value);
                        }
                        _ => {
                            return Err(Error::ReadDatatypeMismatch {
                                src_line_number: self.state.current_line_number,
                            })
                        }
                    }
                }
            }
            self.state.data_pointer += 1;
        }
        Ok(())
    }

    fn evaluate_term<W: Write>(&mut self, term: &Term, stderr: &mut W) -> Result<f64, Error> {
        let mut acc = self.evaluate_factor(&term.factor, stderr)?;
        for (multiplier, factor) in &term.factors {
            let factor = self.evaluate_factor(factor, stderr)?;
            match multiplier {
                Multiplier::Mul => {
                    let res = acc * factor;
                    if res.is_infinite() {
                        self.warn(stderr, "operation overflow (*)");
                    }
                    acc = res
                }
                Multiplier::Div => {
                    if factor == 0.0 {
                        self.warn(stderr, "division by zero ");
                    }
                    acc = acc / factor
                }
            }
        }
        Ok(acc)
    }

    fn evaluate_factor<W: Write>(&mut self, factor: &Factor, stderr: &mut W) -> Result<f64, Error> {
        let mut acc = self.evaluate_primary(&factor.primaries[0], stderr)?;
        for primary in &factor.primaries[1..] {
            let primary = self.evaluate_primary(primary, stderr)?;
            if acc < 0.0 && primary.fract() != 0.0 {
                return Err(Error::FracPowOfNegValue {
                    src_line_number: self.state.current_line_number,
                    value: acc,
                    exp: primary,
                });
            }

            let res = acc.powf(primary);
            if acc == 0.0 && primary < 0f64 {
                self.warn(
                    stderr,
                    &format!("zero raised to negative value ({} ^ {})", acc, primary),
                );
            } else if res.is_infinite() {
                self.warn(stderr, "operation overflow");
            }

            acc = res;
        }
        Ok(acc)
    }

    fn evaluate_primary<W: Write>(
        &mut self,
        primary: &Primary,
        stderr: &mut W,
    ) -> Result<f64, Error> {
        let value = match primary {
            Primary::Variable(v) => self.evaluate_numeric_variable(v, stderr)?,
            Primary::Constant(c) => self.evaluate_numeric_constant(c, stderr)?,
            Primary::Function(f) => self.evaluate_function(f, stderr)?,
            Primary::Expression(e) => self.evaluate_numeric_expression(e, stderr)?,
        };
        Ok(value)
    }

    fn evaluate_function<W: Write>(&mut self, f: &Function, stderr: &mut W) -> Result<f64, Error> {
        match f {
            Function::Abs(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.abs()),
            Function::Atn(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.atan()),
            Function::Cos(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.cos()),
            Function::Exp(expr) => self.evaluate_numeric_expression(expr, stderr).map(|value| {
                let res = value.exp();
                if res.is_infinite() {
                    self.warn(
                        stderr,
                        format!(
                            "operation overflow EXP({})",
                            (value * 100.0).round() / 100.0
                        ),
                    );
                }
                res
            }),
            Function::Int(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.trunc()),
            Function::Rnd => Ok(self.state.rng.gen()),
            Function::Log(expr) => {
                self.evaluate_numeric_expression(expr, stderr)
                    .and_then(|value| {
                        if value <= 0.0 {
                            Err(Error::FunctionDomainError {
                                src_line_number: self.state.current_line_number,
                                function: "LOG".into(),
                                arg: value,
                            })
                        } else {
                            Ok(value.ln())
                        }
                    })
            }
            Function::Sgn(expr) => self.evaluate_numeric_expression(expr, stderr).map(|value| {
                if value == 0.0 {
                    0.0
                } else {
                    value.signum()
                }
            }),
            Function::Sin(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.sin()),
            Function::Sqr(expr) => {
                self.evaluate_numeric_expression(expr, stderr)
                    .and_then(|value| {
                        if value < 0.0 {
                            Err(Error::FunctionDomainError {
                                src_line_number: self.state.current_line_number,
                                function: "SQR".into(),
                                arg: value,
                            })
                        } else {
                            Ok(value.sqrt())
                        }
                    })
            }
            Function::Tan(expr) => self
                .evaluate_numeric_expression(expr, stderr)
                .map(|value| value.tan()),
        }
    }

    fn evaluate_numeric_variable<W: Write>(
        &mut self,
        variable: &NumericVariable,
        stderr: &mut W,
    ) -> Result<f64, Error> {
        let variable = self.make_plain(variable, stderr)?;
        match variable {
            PlainNumericVariable::Simple(v) => {
                if v.digit.is_none() {
                    if let Some(dim) = self.program.array_dims.get(&v.letter) {
                        let info = if dim.dim2.is_some() {
                            "it was previously used or DIM as a two-dimension array"
                        } else {
                            "it was previously used or DIM as a one-dimension array"
                        };
                        return Err(Error::TypeMismatch {
                            src_line_number: self.state.current_line_number,
                            variable: format!("{}", v.letter),
                            info: info.into(),
                        });
                    }
                }

                Ok(self
                    .state
                    .numeric_values
                    .get(&variable)
                    .cloned()
                    .unwrap_or(0f64))
            }
            PlainNumericVariable::Increment(IncrementVariable { line_number })
            | PlainNumericVariable::Limit(LimitVariable { line_number }) => {
                let res = self.state.numeric_values.get(&variable).cloned();
                res.ok_or_else(|| Error::JumpIntoFor {
                    src_line_number: line_number,
                })
            }
            PlainNumericVariable::Array(ar) => Ok(self.get_array_value(&ar)),
        }
    }

    fn evaluate_numeric_constant<W: Write>(
        &self,
        constant: &NumericConstant,
        stderr: &mut W,
    ) -> Result<f64, Error> {
        if constant.significand == 0.0 && constant.exrad < 0 {
            self.warn(
                stderr,
                &format!(
                    "zero raised to negative value ({} * {})",
                    constant.significand, constant.exrad
                ),
            );
        }
        let c = constant.sign * constant.significand * 10f64.powi(constant.exrad);
        if c.is_infinite() {
            let line = self.get_source_line(self.state.current_source_offset);
            let significand = format!("{}", constant.significand as usize);
            let cursor = line.find(&significand);
            if let Some(cursor) = cursor {
                self.warn_with_cursor(stderr, "numeric constant overflow ", cursor);
            } else {
                self.warn(stderr, "numeric constant overflow ");
            }
        }
        Ok(c)
    }

    fn evaluate_on_goto<W: Write>(
        &mut self,
        statement: &OnGotoStatement,
        stderr: &mut W,
    ) -> Result<u16, Error> {
        let result_index = self
            .evaluate_numeric_expression(&statement.numeric_expression, stderr)?
            .round() as usize;
        if result_index < 1 || result_index > statement.line_numbers.len() {
            Err(Error::IndexOutOfRange {
                src_line_number: self.state.current_line_number,
            })
        } else {
            Ok(statement.line_numbers[result_index - 1])
        }
    }

    fn make_plain<W: Write>(
        &mut self,
        variable: &NumericVariable,
        stderr: &mut W,
    ) -> Result<PlainNumericVariable, Error> {
        let plain_variable = match variable {
            NumericVariable::Simple(v) => PlainNumericVariable::Simple(*v),
            NumericVariable::Limit(v) => PlainNumericVariable::Limit(*v),
            NumericVariable::Increment(v) => PlainNumericVariable::Increment(*v),
            NumericVariable::Array(ar) => {
                PlainNumericVariable::Array(self.make_array_plain(ar, stderr)?.0)
            }
        };
        Ok(plain_variable)
    }

    fn make_array_plain<W: Write>(
        &mut self,
        ar: &ArrayVariable,
        stderr: &mut W,
    ) -> Result<(PlainArrayVariable, usize), Error> {
        if self
            .state
            .numeric_values
            .get(&PlainNumericVariable::Simple(SimpleNumericVariable {
                letter: ar.letter,
                digit: None,
            })).is_some()
        {
            let info = "it was previously used as a numeric variable";
            return Err(Error::TypeMismatch {
                src_line_number: self.state.current_line_number,
                variable: format!("{}", ar.letter),
                info: info.into(),
            });
        }

        let subscript1 = self
            .evaluate_numeric_expression(&ar.subscript.0, stderr)?
            .round() as isize;
        let base = self.program.array_base as isize;
        if subscript1 < base || subscript1 >= self.program.array_dims[&ar.letter].dim1 as isize {
            return Err(Error::ArrayIndexOutOfRange {
                src_line_number: self.state.current_line_number,
                array: format!(
                    "{}({}{})",
                    ar.letter,
                    subscript1,
                    ar.subscript.1.as_ref().map(|_| ",...").unwrap_or("")
                ),
            });
        }

        let dim = &self.program.array_dims[&ar.letter];
        if subscript1 >= dim.dim1 as isize {
            return Err(Error::ArrayIndexOutOfRange {
                src_line_number: self.state.current_line_number,
                array: format!(
                    "{}({}{})",
                    ar.letter,
                    subscript1,
                    ar.subscript.1.as_ref().map(|_| ",...").unwrap_or("")
                ),
            });
        }

        if ar.subscript.1.is_some() != dim.dim2.is_some() {
            let info = if dim.dim2.is_some() {
                "it was previously used or DIM as a two-dimension array"
            } else {
                "it was previously used or DIM as a one-dimension array"
            };

            return Err(Error::TypeMismatch {
                src_line_number: self.state.current_line_number,
                variable: format!("{}", ar.letter),
                info: info.into(),
            });
        }

        let subscript2 = if let Some(ref v) = ar.subscript.1 {
            let subscript2 = self.evaluate_numeric_expression(v, stderr)?.round() as isize;
            if subscript2 < base || subscript2 >= dim.dim2.unwrap_or(0) as isize {
                return Err(Error::ArrayIndexOutOfRange {
                    src_line_number: self.state.current_line_number,
                    array: format!("{}(...,{})", ar.letter, subscript2),
                });
            }
            Some(subscript2)
        } else {
            None
        };

        let subscript1 = subscript1 as usize;
        let subscript2 = subscript2.map(|value| value as usize);

        Ok((
            PlainArrayVariable {
                letter: ar.letter,
                subscript: (subscript1, subscript2),
            },
            dim.offset + subscript1 + dim.dim1 * subscript2.unwrap_or(0),
        ))
    }

    // Helper functions

    fn get_array_value(&self, ar: &PlainArrayVariable) -> f64 {
        let index = self.get_array_index(ar);
        self.state.array_values[index]
    }

    fn get_array_index(&self, ar: &PlainArrayVariable) -> usize {
        let dim = &self.program.array_dims[&ar.letter];
        let index = dim.offset + ar.subscript.0 + dim.dim1 * ar.subscript.1.unwrap_or(0);
        index
    }

    fn get_source_line(&self, offset: usize) -> &str {
        self.source_code[offset..].lines().next().unwrap()
    }

    fn warn<W: Write, S: AsRef<str>>(&self, stderr: &mut W, message: S) {
        write!(
            stderr,
            "{}: warning: {}\n",
            self.state.current_line_number,
            message.as_ref()
        );
    }

    /// Generate a warning with statement source code and cursor.
    ///
    /// `cursor_fragment` must be a substring of the current statement source code.
    /// At the beginning of the substring a cursor marker `^` is generated.
    fn warn_with_cursor<W: Write, S: AsRef<str>>(&self, stderr: &mut W, message: S, cursor: usize) {
        let statement_source = self.get_source_line(self.state.current_source_offset as usize);
        write!(
            stderr,
            "{}: warning: {}\n {}\n {:cursor$}^\n",
            self.state.current_line_number,
            message.as_ref(),
            statement_source,
            "",
            cursor = cursor
        );
    }
}
