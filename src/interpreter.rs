use ast::*;
use error::Error;
use format::format_float;
use itertools::Itertools;

use std::collections::HashMap;
use std::io::Write;

#[derive(Debug, Default)]
struct State {
    numeric_values: HashMap<NumericVariable, f64>,
    string_values: HashMap<StringVariable, String>,
    stack: Vec<u16>,
    columnar_position: usize,
}

fn evaluate_numeric_variable(variable: &NumericVariable, state: &State) -> Result<f64, Error> {
    state
        .numeric_values
        .get(variable)
        .cloned()
        .ok_or_else(|| Error::UndefinedNumericVariable(*variable))
}

fn evaluate_primary(primary: &Primary, state: &State) -> Result<f64, Error> {
    let value = match primary {
        Primary::Variable(v) => evaluate_numeric_variable(v, state)?,
        Primary::Constant(c) => *c,
        Primary::Expression(e) => evaluate_numeric_expression(e, state)?,
    };
    Ok(value)
}

fn evaluate_factor(factor: &Factor, state: &State) -> Result<f64, Error> {
    let mut primaries = factor.primaries.iter().map(|p| evaluate_primary(p, state));
    let first_primary = primaries.next().expect("logic error")?;
    primaries.fold_results(first_primary, |acc, primary| acc.powf(primary))
}

fn evaluate_term(term: &Term, state: &State) -> Result<f64, Error> {
    let first_factor = evaluate_factor(&term.factor, state)?;
    term.factors
        .iter()
        .map(|(m, f)| evaluate_factor(f, state).map(|f| (m, f)))
        .fold_results(first_factor, |acc, (multiplier, factor)| match multiplier {
            Multiplier::Mul => acc * factor,
            Multiplier::Div => acc / factor,
        })
}

fn evaluate_numeric_expression(
    expression: &NumericExpression,
    state: &State,
) -> Result<f64, Error> {
    expression
        .terms
        .iter()
        .map(|(sign, term)| evaluate_term(term, state).map(|term| (sign, term)))
        .fold_results(0.0, |acc, (sign, term)| acc + *sign * term)
}

// TODO: Replace output with a Writer.
fn evaluate_print<W>(
    line_number: u16,
    statement: &PrintStatement,
    state: &mut State,
    output: &mut W,
    err_output: &mut W,
) -> Result<(), Error>
where
    W: Write,
{
    const COLUMN_WIDTH: usize = 70;
    const NUM_PRINT_ZONES: usize = 5;
    const PRINT_ZONE_WIDTH: usize = COLUMN_WIDTH / NUM_PRINT_ZONES;

    let mut columnar_position = state.columnar_position;
    for item in &statement.list {
        match item {
            PrintItem::Expression(expression) => match expression {
                Expression::String(string_expression) => {
                    let value = evaluate_string_expression(string_expression, state)?;
                    write!(output, "{}", value);
                    columnar_position += value.len();
                }
                Expression::Numeric(numeric_expression) => {
                    let value = evaluate_numeric_expression(numeric_expression, state)?;
                    let value_str = format_float(value);
                    write!(output, "{}", value_str);
                    columnar_position += value_str.len();
                }
            },
            PrintItem::TabCall(numeric_expression) => {
                let tab_width =
                    evaluate_numeric_expression(numeric_expression, state)?.round() as i64;
                if tab_width < 1 {
                    write!(
                        err_output,
                        "{}: warning: invalid TAB argument ({})\n",
                        line_number, tab_width
                    );
                    continue;
                }

                let tab_width = (tab_width as usize) % COLUMN_WIDTH;
                if tab_width < columnar_position {
                    write!(output, "\n");
                    columnar_position = 0;
                }

                write!(output, "{:1$}", "", tab_width - columnar_position - 1);
                columnar_position = tab_width - 1;
            }
            PrintItem::Comma => {
                let current_print_zone = columnar_position / PRINT_ZONE_WIDTH;
                if current_print_zone + 1 < NUM_PRINT_ZONES {
                    let next_columnar_position = (current_print_zone + 1) * PRINT_ZONE_WIDTH;
                    write!(
                        output,
                        "{:1$}",
                        "",
                        next_columnar_position - columnar_position
                    );
                    columnar_position = next_columnar_position;
                } else {
                    write!(output, "\n");
                }
            }
            PrintItem::Semicolon => (),
        }
    }
    state.columnar_position = columnar_position;

    let last_item_is_comma_or_semicolon = statement
        .list
        .last()
        .map(|s| match s {
            PrintItem::Semicolon => true,
            PrintItem::Comma => true,
            _ => false,
        }).unwrap_or(false);
    if !last_item_is_comma_or_semicolon {
        state.columnar_position = 0;
        write!(output, "\n");
    }

    Ok(())
}

fn evaluate_string_expression<'a>(
    expression: &'a StringExpression,
    state: &'a State,
) -> Result<&'a str, Error> {
    match expression {
        StringExpression::Variable(variable) => {
            let value = state
                .string_values
                .get(variable)
                .ok_or_else(|| Error::UndefinedStringVariable(*variable))?;
            Ok(value)
        }
        StringExpression::Constant(constant) => Ok(&constant.0),
    }
}

fn evaluate_let(statement: &LetStatement, state: &mut State) -> Result<(), Error> {
    match statement {
        LetStatement::Numeric {
            variable,
            expression,
        } => {
            let value = evaluate_numeric_expression(expression, state)?;
            state.numeric_values.insert(*variable, value);
        }
        LetStatement::String {
            variable,
            expression,
        } => {
            let value = String::from(evaluate_string_expression(expression, state)?);
            state.string_values.insert(*variable, value);
        }
    }

    Ok(())
}

fn evaluate_if(
    left_expression: &Expression,
    relation: &Relationship,
    right_expression: &Expression,
    line_number: &u16,
    state: &State,
) -> Result<bool, Error> {
    match (left_expression, right_expression) {
        (
            Expression::Numeric(left_numeric_expression),
            Expression::Numeric(right_numeric_expression),
        ) => {
            let left = evaluate_numeric_expression(left_numeric_expression, state)?;
            let right = evaluate_numeric_expression(right_numeric_expression, state)?;

            Ok(match relation {
                Relationship::LessThan => left < right,
                Relationship::LessThanOrEqualTo => left <= right,
                Relationship::EqualTo => left == right,
                Relationship::GreaterThanOrEqualTo => left >= right,
                Relationship::GreaterThan => left > right,
                Relationship::NotEqualTo => left != right,
            })
        }
        (
            Expression::String(left_string_expression),
            Expression::String(right_string_expression),
        ) => {
            let left = evaluate_string_expression(left_string_expression, state)?;
            let right = evaluate_string_expression(right_string_expression, state)?;

            Ok(match relation {
                Relationship::LessThan => left < right,
                Relationship::LessThanOrEqualTo => left <= right,
                Relationship::EqualTo => left == right,
                Relationship::GreaterThanOrEqualTo => left >= right,
                Relationship::GreaterThan => left > right,
                Relationship::NotEqualTo => left != right,
            })
        }
        _ => Err(Error::InvalidIfStatement {
            src_line_number: *line_number,
        }),
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

// Return value `true` means evalution should continue.
fn evaluate_statement<W>(
    line_number: u16,
    statement: &Statement,
    state: &mut State,
    output: &mut W,
    err_output: &mut W,
) -> Result<Action, Error>
where
    W: Write,
{
    let res = match statement {
        Statement::Print(statement) => {
            evaluate_print(line_number, statement, state, output, err_output)?;
            Action::NextLine
        }
        Statement::Let(statement) => {
            evaluate_let(statement, state)?;
            Action::NextLine
        }
        Statement::Goto(line_number) => Action::Goto(*line_number),
        Statement::Gosub(line_number) => Action::Gosub(*line_number),
        Statement::If(left_expression, relationship, right_expression, line_number) => {
            if evaluate_if(
                left_expression,
                relationship,
                right_expression,
                line_number,
                state,
            )? {
                Action::Goto(*line_number)
            } else {
                Action::NextLine
            }
        }
        Statement::Rem => Action::NextLine,
        Statement::Return => Action::Return,
        Statement::Stop => Action::Stop,
        Statement::End => Action::Stop,
    };
    Ok(res)
}

pub fn evaluate(program: &Program) -> Result<(String, String), Error> {
    let mut state = State::default();
    let mut output = Vec::new();
    let mut err_output = Vec::new();

    let mut block = program.first_block();
    loop {
        let (action, src_line_number) = match block {
            Block::Line {
                line_number,
                statement,
            } => (
                evaluate_statement(
                    *line_number,
                    statement,
                    &mut state,
                    &mut output,
                    &mut err_output,
                )?,
                *line_number,
            ),
        };

        match action {
            Action::NextLine => block = program.next_block(block),
            Action::Goto(line_number) => {
                block = program
                    .get_block_by_line_number(line_number)
                    .ok_or_else(|| Error::UndefinedLineNumber {
                        src_line_number,
                        line_number,
                    })?;
            }
            Action::Gosub(line_number) => {
                block = program
                    .get_block_by_line_number(line_number)
                    .ok_or_else(|| Error::UndefinedLineNumber {
                        src_line_number,
                        line_number,
                    })?;
                state.stack.push(src_line_number);
            }
            Action::Return => {
                let prev_line_number = state
                    .stack
                    .pop()
                    .ok_or_else(|| Error::UnexpectedReturn { src_line_number })?;
                let prev_block = program
                    .get_block_by_line_number(prev_line_number)
                    .ok_or_else(|| Error::UndefinedLineNumber {
                        src_line_number,
                        line_number: prev_line_number,
                    })?;
                block = program.next_block(prev_block);
            }
            Action::Stop => break,
        }
    }

    let o = String::from_utf8(output).unwrap();
    let e = String::from_utf8(err_output).unwrap();
    Ok((o, e))
}
