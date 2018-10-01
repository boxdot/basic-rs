use ast::*;
use error::Error;
use format::format_float;
use parser;

use itertools::Itertools;
use std::collections::HashMap;
use std::io::Write;

pub struct Interpreter<'a> {
    program: &'a Program<'a>,
    source_code: &'a str,
    state: State,
}

#[derive(Debug, Default)]
struct State {
    /// BASIC line number label of the current statement
    current_line_number: u16,
    /// Offset in the source code of the current statement
    current_source_offset: usize,
    /// current columnar position of the cursor
    columnar_position: usize,
    /// values of numeric variables
    numeric_values: HashMap<NumericVariable, f64>,
    /// values of string variables
    string_values: HashMap<StringVariable, String>,
    /// stack of return line numbers for routines
    stack: Vec<u16>,
    /// DATA statement pointer
    data_pointer: u16,
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
            state: State::default(),
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
                        .ok_or_else(|| Error::UnexpectedReturn { src_line_number })?;
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
            Statement::IfThen(if_statement, line_number) => {
                if self.evaluate_if(if_statement, stderr)? {
                    Action::Goto(*line_number)
                } else {
                    Action::NextLine
                }
            }
            Statement::OnGoto(statement) => Action::Goto(self.evaluate_on_goto(statement, stderr)?),
            Statement::For(for_statement) => {
                let initial_value =
                    self.evaluate_numeric_expression(&for_statement.initial_value, stderr)?;
                self.state
                    .numeric_values
                    .insert(for_statement.control_variable, initial_value);
                self.state.stack.push(self.state.current_line_number);
                Action::NextLine
            }
            Statement::Next(control_variable) => {
                let for_statement_line_number = self.state.stack.last().expect("NEXT before FOR");
                let block = self
                    .program
                    .get_block_by_line_number(*for_statement_line_number)
                    .expect("FOR not in stack");
                match block {
                    Block::Line {
                        statement: Statement::For(for_statement),
                        ..
                    } => {
                        let mut control_value = self
                            .state
                            .numeric_values
                            .get_mut(control_variable)
                            .expect("FOR without control variable");
                        let inc =
                            self.evaluate_numeric_expression(&for_statement.increment, stderr)?;
                        *control_value += inc;
                    }
                    _ => panic!("shit"),
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
            Statement::Data(datum) => Action::NextLine,
            Statement::Rem => Action::NextLine,
            Statement::Return => Action::Return,
            Statement::Stop => Action::Stop,
            Statement::End => Action::Stop,
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
                variable,
                expression,
            } => {
                let value = self.evaluate_numeric_expression(expression, stderr)?;
                self.state.numeric_values.insert(*variable, value);
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
        &self,
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
        &self,
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
            let data = self
                .program
                .datum
                .get(self.state.data_pointer as usize)
                .ok_or_else(|| Error::MissingData {
                    src_line_number: self.state.current_line_number,
                })?;
            match (variable, data) {
                (Variable::Numeric(numeric_variable), Expression::Numeric(numeric_expression)) => {
                    let value = self.evaluate_numeric_expression(&numeric_expression, stderr)?;
                    self.state.numeric_values.insert(*numeric_variable, value);
                }
                (Variable::String(string_variable), Expression::String(string_expression)) => {
                    let value = String::from(self.evaluate_string_expression(&string_expression)?);
                    self.state.string_values.insert(*string_variable, value);
                }
                (_, _) => {
                    return Err(Error::ReadDatatypeMismatch {
                        src_line_number: self.state.current_line_number,
                        data_pointer: self.state.data_pointer,
                    })
                }
            }
            self.state.data_pointer += 1;
        }
        Ok(())
    }

    fn evaluate_term<W: Write>(&self, term: &Term, stderr: &mut W) -> Result<f64, Error> {
        let mut acc = self.evaluate_factor(&term.factor, stderr)?;
        for (multiplier, factor) in &term.factors {
            let factor = self.evaluate_factor(factor, stderr)?;
            match multiplier {
                Multiplier::Mul => {
                    let res = acc * factor;
                    if acc.is_normal() && factor.is_normal() && !res.is_normal() {
                        self.warn(stderr, "operation overflow (*) ");
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

    fn evaluate_factor<W: Write>(&self, factor: &Factor, stderr: &mut W) -> Result<f64, Error> {
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
            if acc == 0.0 && primary < 0f64 {
                self.warn(
                    stderr,
                    &format!("zero raised to negative value ({} ^ {})", acc, primary),
                );
            }
            acc = acc.powf(primary);
        }
        Ok(acc)
    }

    fn evaluate_primary<W: Write>(&self, primary: &Primary, stderr: &mut W) -> Result<f64, Error> {
        let value = match primary {
            Primary::Variable(v) => self.evaluate_numeric_variable(v)?,
            Primary::Constant(significand, exrad) => {
                if *significand == 0.0 && *exrad < 0 {
                    self.warn(
                        stderr,
                        &format!(
                            "zero raised to negative value ({} * {})",
                            significand, exrad
                        ),
                    );
                }
                let c = significand * 10f64.powi(*exrad);
                if c.is_infinite() {
                    let line = self.get_source_line(self.state.current_source_offset);
                    let (_, span) =
                        parser::let_statement_numeric_constant_pos(line).expect("parser bug");
                    self.warn_with_cursor(stderr, "numeric constant overflow ", span.offset);
                }
                c
            }
            Primary::Expression(e) => self.evaluate_numeric_expression(e, stderr)?,
        };
        Ok(value)
    }

    fn evaluate_numeric_variable(&self, variable: &NumericVariable) -> Result<f64, Error> {
        Ok(self
            .state
            .numeric_values
            .get(variable)
            .cloned()
            .unwrap_or(0f64))
    }

    fn evaluate_on_goto<W: Write>(
        &self,
        statement: &OnGotoStatement,
        stderr: &mut W,
    ) -> Result<u16, Error> {
        let result_index = self
            .evaluate_numeric_expression(&statement.numeric_expression, stderr)?
            .round() as usize;
        if result_index < 1 || result_index >= statement.line_numbers.len() {
            return Err(Error::InvalidOnGotoValue {
                src_line_number: self.state.current_line_number,
                value: result_index,
            });
        }

        Ok(statement.line_numbers[result_index - 1])
    }

    // Helper functions

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
        println!("cursor: {:?}", cursor);
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
