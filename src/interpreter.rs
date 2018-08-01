use ast::*;

pub fn evaluate(program: &Program) -> String {
    let mut output = String::new();

    for block in &program.blocks {
        match block {
            Block::Line { statement, .. } => match statement {
                Statement::Print(statement) => {
                    for item in &statement.list {
                        match item {
                            PrintItem::Expression(expression) => match expression {
                                Expression::String(string_expression) => match string_expression {
                                    StringExpression::Constant(constant) => {
                                        output += &constant.0;
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
                _ => (),
            },
        }
    }

    output
}
