use ast::*;

fn evaluate_print(statement: &PrintStatement, output: &mut String) {
    for item in &statement.list {
        match item {
            PrintItem::Expression(expression) => match expression {
                Expression::String(string_expression) => match string_expression {
                    StringExpression::Constant(constant) => {
                        *output += &constant.0;
                    }
                    _ => (),
                },
                _ => (),
            },
            _ => (),
        }
    }
    output.push('\n');
}

// Return value `true` means evalution should continue.
fn evaluate_statement(statement: &Statement, output: &mut String) -> bool {
    match statement {
        Statement::Print(statement) => {
            evaluate_print(statement, output);
            true
        }
        Statement::Stop => false,
        Statement::End => false,
    }
}

pub fn evaluate(program: &Program) -> String {
    let mut output = String::new();
    for block in &program.blocks {
        match block {
            Block::Line { statement, .. } => {
                if !evaluate_statement(statement, &mut output) {
                    break;
                }
            }
        }
    }
    output
}
