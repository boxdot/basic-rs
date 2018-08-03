use ast::*;
use error::Error;

fn evaluate_primary(primary: &Primary) -> f64 {
    match primary {
        Primary::Variable(_) => unimplemented!(),
        Primary::Constant(c) => *c,
        Primary::Expression(e) => evaluate_numeric_expression(e),
    }
}

fn evaluate_factor(factor: &Factor) -> f64 {
    let mut primaries = factor.primaries.iter();
    let first_primary = primaries.next().expect("logic error");
    primaries.fold(evaluate_primary(first_primary), |acc, primary| {
        acc.powf(evaluate_primary(primary))
    })
}

fn evaluate_term(term: &Term) -> f64 {
    term.factors.iter().fold(
        evaluate_factor(&term.factor),
        |acc, (multiplier, factor)| {
            let factor = evaluate_factor(&factor);
            match multiplier {
                Multiplier::Mul => acc * factor,
                Multiplier::Div => acc / factor,
            }
        },
    )
}

fn evaluate_numeric_expression(expression: &NumericExpression) -> f64 {
    expression
        .terms
        .iter()
        .fold(0.0, |acc, (sign, term)| acc + *sign * evaluate_term(term))
}

// TODO: Replace output with a Writer.
fn evaluate_print(statement: &PrintStatement, output: &mut String) -> Result<(), Error> {
    const COLUMN_WIDTH: usize = 70;
    const NUM_PRINT_ZONES: usize = 5;
    const PRINT_ZONE_WIDTH: usize = COLUMN_WIDTH / NUM_PRINT_ZONES;

    let mut columnar_position = 0;
    for item in &statement.list {
        match item {
            PrintItem::Expression(expression) => match expression {
                Expression::String(string_expression) => match string_expression {
                    StringExpression::Constant(constant) => {
                        *output += &constant.0;
                        columnar_position += constant.0.len();
                    }
                    _ => (),
                },
                _ => (),
            },
            PrintItem::TabCall(numeric_expression) => {
                let tab_width = evaluate_numeric_expression(numeric_expression) as usize;
                if tab_width == 0 {
                    return Err(Error::InvalidTabCall);
                }

                let tab_width = tab_width % COLUMN_WIDTH;
                if tab_width < columnar_position {
                    output.push('\n');
                    columnar_position = 0;
                }

                *output += &" ".repeat(tab_width - 1);
                columnar_position += tab_width - 1;
            }
            PrintItem::Comma => {
                let current_print_zone = columnar_position / PRINT_ZONE_WIDTH;
                if current_print_zone + 1 < NUM_PRINT_ZONES {
                    let next_columnar_position = (current_print_zone + 1) * PRINT_ZONE_WIDTH;
                    *output += &" ".repeat(next_columnar_position - columnar_position);
                    columnar_position = next_columnar_position;
                } else {
                    output.push('\n');
                }
            }
            PrintItem::Semicolon => (),
        }
    }

    let last_item_is_semicolon = statement
        .list
        .last()
        .map(|s| match s {
            PrintItem::Semicolon => true,
            _ => false,
        })
        .unwrap_or(false);
    if !last_item_is_semicolon {
        output.push('\n');
    }

    Ok(())
}

// Return value `true` means evalution should continue.
fn evaluate_statement(statement: &Statement, output: &mut String) -> Result<bool, Error> {
    let res = match statement {
        Statement::Print(statement) => {
            evaluate_print(statement, output)?;
            true
        }
        Statement::Let(_) => true,
        Statement::Stop => false,
        Statement::End => false,
    };
    Ok(res)
}

pub fn evaluate(program: &Program) -> Result<String, Error> {
    let mut output = String::new();
    for block in &program.blocks {
        match block {
            Block::Line { statement, .. } => {
                if !evaluate_statement(statement, &mut output)? {
                    break;
                }
            }
        }
    }
    Ok(output)
}
