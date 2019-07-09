use crate::ast;
use crate::error;

use nom5::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1, take_while_m_n},
    character::complete::{char, one_of, space0, space1},
    combinator::{map, map_res, opt},
    error::ParseError,
    multi::{many0, separated_nonempty_list},
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};

fn full_stop<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    char('.')(i)
}

fn is_space(c: char) -> bool {
    c == ' '
}

fn quotation_mark<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    char('"')(i)
}

// 4. Characters and Strings
//
// Each rule taking a single character is implemented as a matching rule and as boolean check.
// The latter is needed for functions consuming character as long a predicate is satisfied, like
// `take_while`, `take_while1`, etc...

fn letter<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")(i)
}

fn is_letter(c: char) -> bool {
    match c {
        'A'..='Z' => true,
        _ => false,
    }
}

fn digit<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    one_of("0123456789")(i)
}

fn is_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn is_string_character(c: char) -> bool {
    c == '"' || is_quoted_string_character(c)
}

fn is_quoted_string_character(c: char) -> bool {
    match c {
        // quoted_string_character
        '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | ',' | '/' | ':' | ';' | '<'
        | '=' | '>' | '?' | '^' | '_' => true,
        other => is_unquoted_string_character(other),
    }
}

fn is_unquoted_string_character(c: char) -> bool {
    is_space(c) || is_plain_string_character(c)
}

fn plain_string_character<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    alt((one_of("+-."), digit, letter))(i)
}

fn is_plain_string_character(c: char) -> bool {
    match c {
        '+' | '-' | '.' => true,
        other => is_digit(other) || is_letter(other),
    }
}

fn remark_string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(is_string_character)(i)
}

fn quoted_string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(
        quotation_mark,
        terminated(take_while(is_quoted_string_character), quotation_mark),
    )(i)
}

fn unquoted_string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    alt((
        preceded(
            plain_string_character,
            terminated(
                take_while(is_unquoted_string_character),
                plain_string_character,
            ),
        ),
        take_while_m_n(1, 1, is_plain_string_character),
    ))(i)
}

// 5. Programs

pub fn program<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Result<ast::Program<'a>, error::Error>, E> {
    map(terminated(many0(block), end_of_line), |blocks| {
        ast::Program::new(blocks, i)
    })(i)
}

// FIXME: This rule is different from the rule in the spec in the sense that a block consists of
// many lines and not a single one.
fn block<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block<'a>, E> {
    alt((line, for_block))(i)
}

pub fn line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block<'a>, E> {
    map(
        tuple((
            terminated(line_number, space1),
            terminated(statement, space0),
            end_of_line,
        )),
        |(line_number, statement, _)| ast::Block::Line {
            line_number,
            statement,
            statement_source: i,
        },
    )(i)
}

fn line_number<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, u16, E> {
    map_res(take_while_m_n(1, 4, is_digit), |digits| {
        u16::from_str_radix(digits, 10)
    })(i)
}

fn end_of_line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    if i.len() == 0 {
        Ok((i, ())) // EOF
    } else {
        map(char('\n'), |_| ())(i)
    }
}

fn end_line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block, E> {
    map(
        tuple((
            terminated(line_number, space1),
            terminated(end_statement, space0),
            end_of_line,
        )),
        |(line_number, statement, _)| ast::Block::Line {
            line_number,
            statement,
            statement_source: i,
        },
    )(i)
}

fn end_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(tag("END"), |_| ast::Statement::End)(i)
}

fn statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    alt((
        goto_statement,
        gosub_statement,
        on_goto_statement,
        if_then_statement,
        def_statement,
        let_statement,
        print_statement,
        return_statement,
        stop_statement,
        input_statement,
        read_statement,
        restore_statement,
        data_statement,
        remark_statement,
        dimension_statement,
        option_statement,
        randomize_statement,
        end_statement,
    ))(i)
}

// 6. Constants

pub fn numeric_constant<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::NumericConstant, E> {
    map(pair(opt(sign), numeric_rep), |(sign, numeric_rep)| {
        ast::NumericConstant {
            sign: sign.unwrap_or(ast::Sign::Pos),
            significand: numeric_rep.0,
            exrad: numeric_rep.1,
        }
    })(i)
}

fn sign<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Sign, E> {
    alt((
        map(char('+'), |_| ast::Sign::Pos),
        map(char('-'), |_| ast::Sign::Neg),
    ))(i)
}

fn numeric_rep<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (f64, i32), E> {
    map(
        tuple((opt(sign), significand, opt(exrad))),
        |(sign, significand, exrad)| {
            (
                sign.unwrap_or(ast::Sign::Pos) * significand,
                exrad.unwrap_or(0),
            )
        },
    )(i)
}

fn significand<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, f64, E> {
    alt((
        map(pair(opt(integer), fraction), |(integer, fraction)| {
            integer.unwrap_or(0) as f64 + fraction
        }),
        map(terminated(integer, opt(full_stop)), |integer| {
            integer as f64
        }),
    ))(i)
}

fn integer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, u64, E> {
    map_res(take_while1(is_digit), |digits| {
        u64::from_str_radix(digits, 10)
    })(i)
}

fn fraction<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, f64, E> {
    map_res(preceded(full_stop, take_while1(is_digit)), |digits| {
        u64::from_str_radix(digits, 10)
            .map(|frac| frac as f64 / (10_u64.pow(digits.len() as u32) as f64))
    })(i)
}

fn exrad<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, i32, E> {
    map(
        preceded(char('E'), pair(opt(sign), integer)),
        |(sign, exp)| sign.unwrap_or(ast::Sign::Pos) * exp as i32,
    )(i)
}

fn string_constant<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::StringConstant, E> {
    // TODO: Avoid cloning here
    map(quoted_string, |s| ast::StringConstant(s.to_string()))(i)
}

// 7. Variables

fn variable<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Variable, E> {
    alt((
        map(string_variable, ast::Variable::String),
        map(numeric_variable, ast::Variable::Numeric),
    ))(i)
}

fn numeric_variable<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::NumericVariable, E> {
    alt((
        map(numeric_array_element, ast::NumericVariable::Array),
        map(simple_numeric_variable, ast::NumericVariable::Simple),
    ))(i)
}

fn simple_numeric_variable<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::SimpleNumericVariable, E> {
    map(
        pair(letter, opt(map(digit, |c| c as u8 - b'0'))),
        |(letter, digit)| ast::SimpleNumericVariable { letter, digit },
    )(i)
}

fn numeric_array_element<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::ArrayVariable, E> {
    map(
        pair(numeric_array_name, subscript),
        |(letter, subscript)| ast::ArrayVariable { letter, subscript },
    )(i)
}

fn numeric_array_name<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    letter(i)
}

fn subscript<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (ast::NumericExpression, Option<ast::NumericExpression>), E> {
    let inner = pair(
        numeric_expression,
        opt(preceded(char(','), numeric_expression)),
    );
    preceded(char('('), terminated(inner, char(')')))(i)
}

fn string_variable<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::StringVariable, E> {
    map(terminated(letter, char('$')), ast::StringVariable)(i)
}

// 8. Expressions

fn expression<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Expression, E> {
    alt((
        map(string_expression, ast::Expression::String),
        map(numeric_expression, ast::Expression::Numeric),
    ))(i)
}

fn numeric_expression<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::NumericExpression, E> {
    let leading_sign = terminated(opt(sign), space0);
    let leading_term = term;
    let terms = many0(pair(preceded(space0, sign), preceded(space0, term)));
    map(
        tuple((leading_sign, leading_term, terms)),
        |(leading_sign, leading_term, terms)| {
            ast::NumericExpression::new(leading_sign, leading_term, terms)
        },
    )(i)
}

fn term<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Term, E> {
    let factors = many0(pair(preceded(space0, multiplier), preceded(space0, factor)));
    map(pair(factor, factors), |(leading_factor, factors)| {
        ast::Term::new(leading_factor, factors)
    })(i)
}

fn factor<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Factor, E> {
    let primaries = many0(preceded(
        preceded(space0, char('^')),
        preceded(space0, primary),
    ));
    map(pair(primary, primaries), |(leading_primary, primaries)| {
        ast::Factor::new(leading_primary, primaries)
    })(i)
}

fn multiplier<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Multiplier, E> {
    alt((
        map(char('*'), |_| ast::Multiplier::Mul),
        map(char('/'), |_| ast::Multiplier::Div),
    ))(i)
}

fn primary<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Primary, E> {
    alt((
        map(numeric_function_ref, ast::Primary::Function),
        map(numeric_defined_function_ref, ast::Primary::DefFunctionCall),
        map(numeric_variable, ast::Primary::Variable),
        map(numeric_rep, |value| {
            ast::Primary::Constant(ast::NumericConstant::from(value))
        }),
        map(
            preceded(char('('), terminated(numeric_expression, char(')'))),
            ast::Primary::Expression,
        ),
    ))(i)
}

fn numeric_function_ref<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Function, E> {
    alt((
        map(preceded(tag("ABS"), argument_list), ast::Function::Abs),
        map(preceded(tag("ATN"), argument_list), ast::Function::Atn),
        map(preceded(tag("COS"), argument_list), ast::Function::Cos),
        map(preceded(tag("EXP"), argument_list), ast::Function::Exp),
        map(preceded(tag("INT"), argument_list), ast::Function::Int),
        map(preceded(tag("LOG"), argument_list), ast::Function::Log),
        map(tag("RND"), |_| ast::Function::Rnd),
        map(preceded(tag("SGN"), argument_list), ast::Function::Sgn),
        map(preceded(tag("SIN"), argument_list), ast::Function::Sin),
        map(preceded(tag("SQR"), argument_list), ast::Function::Sqr),
        map(preceded(tag("TAN"), argument_list), ast::Function::Tan),
    ))(i)
}

fn numeric_defined_function_ref<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::DefFunctionCall, E> {
    map(
        pair(numeric_defined_function, opt(argument_list)),
        |(name, arg)| ast::DefFunctionCall { name, arg },
    )(i)
}

fn argument_list<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::NumericExpression, E> {
    preceded(char('('), terminated(numeric_expression, char(')')))(i)
}

fn string_expression<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::StringExpression, E> {
    alt((
        map(string_variable, ast::StringExpression::Variable),
        map(string_constant, ast::StringExpression::Constant),
    ))(i)
}

// 9. Implementation supplied functions
//
// The only rule `numeric_supplied_function` is merged with `numeric_function_ref`.

// 10. User defined functions

fn def_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let def = terminated(tag("DEF"), space1);
    let equal_sign = preceded(space0, terminated(char('='), space0));
    map(
        tuple((
            def,
            numeric_defined_function,
            opt(parameter_list),
            equal_sign,
            numeric_expression,
        )),
        |(_, name, parameter, _, expression)| {
            ast::Statement::Def(ast::DefFunction {
                name,
                parameter,
                expression,
            })
        },
    )(i)
}

fn numeric_defined_function<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    preceded(tag("FN"), letter)(i)
}

fn parameter_list<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::SimpleNumericVariable, E> {
    preceded(char('('), terminated(parameter, char(')')))(i)
}

fn parameter<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::SimpleNumericVariable, E> {
    simple_numeric_variable(i)
}

// 11. LET statement

fn let_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(
        alt((numeric_let_statement, string_let_statement)),
        ast::Statement::Let,
    )(i)
}

fn numeric_let_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::LetStatement, E> {
    let let_tag = terminated(tag("LET"), space1);
    let equal_sign = preceded(space0, terminated(char('='), space0));
    map(
        tuple((let_tag, numeric_variable, equal_sign, numeric_expression)),
        |(_, variable, _, expression)| ast::LetStatement::Numeric {
            variable,
            expression,
        },
    )(i)
}

fn string_let_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::LetStatement, E> {
    let let_tag = terminated(tag("LET"), space1);
    let equal_sign = preceded(space0, terminated(char('='), space0));
    map(
        tuple((let_tag, string_variable, equal_sign, string_expression)),
        |(_, variable, _, expression)| ast::LetStatement::String {
            variable,
            expression,
        },
    )(i)
}

// 12. Control statements

fn goto_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let go = terminated(tag("GO"), space0);
    let to = terminated(tag("TO"), space1);
    map(
        preceded(go, preceded(to, line_number)),
        ast::Statement::Goto,
    )(i)
}

fn if_then_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Statement, E> {
    let if_tag = terminated(tag("IF"), space1);
    let if_statement = terminated(relational_expression, space1);
    let then_tag = terminated(tag("THEN"), space1);
    map(
        tuple((if_tag, if_statement, then_tag, line_number)),
        |(_, if_statement, _, line_number)| ast::Statement::IfThen(if_statement, line_number),
    )(i)
}

fn relational_expression<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::RelationalExpression, E> {
    let relation = preceded(space1, terminated(relation, space1));
    let equality_relation = preceded(space1, terminated(equality_relation, space1));
    alt((
        map(
            tuple((numeric_expression, relation, numeric_expression)),
            |(left_expression, relation, right_expression)| {
                ast::RelationalExpression::NumericComparison(
                    left_expression,
                    relation,
                    right_expression,
                )
            },
        ),
        map(
            tuple((string_expression, equality_relation, string_expression)),
            |(left_expression, relation, right_expression)| {
                ast::RelationalExpression::StringComparison(
                    left_expression,
                    relation,
                    right_expression,
                )
            },
        ),
    ))(i)
}

fn relation<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Relation, E> {
    alt((
        map(tag("=="), |_| ast::Relation::EqualTo),
        map(tag("="), |_| ast::Relation::EqualTo),
        map(tag("<>"), |_| ast::Relation::NotEqualTo),
        map(tag("<="), |_| ast::Relation::LessThanOrEqualTo),
        map(tag(">="), |_| ast::Relation::GreaterThanOrEqualTo),
        map(tag("<"), |_| ast::Relation::LessThan),
        map(tag(">"), |_| ast::Relation::GreaterThan),
    ))(i)
}

fn equality_relation<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::EqualityRelation, E> {
    alt((
        map(tag("=="), |_| ast::EqualityRelation::EqualTo),
        map(tag("="), |_| ast::EqualityRelation::EqualTo),
        map(tag("<>"), |_| ast::EqualityRelation::NotEqualTo),
    ))(i)
}

fn gosub_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let go = terminated(tag("GO"), space0);
    let to = terminated(tag("SUB"), space1);
    map(
        preceded(go, preceded(to, line_number)),
        ast::Statement::Gosub,
    )(i)
}

fn return_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(tag("RETURN"), |_| ast::Statement::Return)(i)
}

fn on_goto_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Statement, E> {
    let on = terminated(tag("ON"), space1);
    let go = terminated(tag("GO"), space1);
    let to = terminated(tag("TO"), space1);

    let numeric_expression = terminated(numeric_expression, space1);
    let line_numbers =
        separated_nonempty_list(preceded(space0, terminated(char(','), space0)), line_number);

    map(
        pair(
            preceded(on, numeric_expression),
            preceded(go, preceded(to, line_numbers)),
        ),
        |(numeric_expression, line_numbers)| {
            ast::Statement::OnGoto(ast::OnGotoStatement {
                numeric_expression,
                line_numbers,
            })
        },
    )(i)
}

fn stop_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(tag("STOP"), |_| ast::Statement::Stop)(i)
}

// 13. FOR and NEXT statements

fn for_block<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block, E> {
    map(
        pair(for_line, for_body),
        |(for_line, (blocks, next_line))| ast::Block::For {
            for_line,
            blocks,
            next_line,
        },
    )(i)
}

fn for_body<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (Vec<ast::Block>, ast::NextLine), E> {
    // TODO: Note that our block definition always contains a single line, therefore we parse
    // several blocks here, which is different to the rule from the spec.
    pair(many0(block), next_line)(i)
}

fn for_line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::ForLine, E> {
    map(
        tuple((
            terminated(line_number, space1),
            terminated(for_statement, space0),
            end_of_line,
        )),
        |(line_number, for_statement, _)| ast::ForLine {
            line_number,
            for_statement,
            statement_source: i,
        },
    )(i)
}

pub fn next_line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::NextLine, E> {
    map(
        tuple((
            terminated(line_number, space1),
            terminated(next_statement, space0),
            end_of_line,
        )),
        |(line_number, next_statement, _)| ast::NextLine {
            line_number,
            next_statement,
            statement_source: i,
        },
    )(i)
}

fn for_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::ForStatement, E> {
    let for_tag = terminated(tag("FOR"), space1);
    let equal_sign = preceded(space0, terminated(char('='), space0));
    let to_tag = preceded(space1, terminated(tag("TO"), space1));
    let step_tag = preceded(space1, terminated(tag("STEP"), space1));

    map(
        tuple((
            preceded(for_tag, simple_numeric_variable),
            preceded(equal_sign, numeric_expression),
            preceded(to_tag, numeric_expression),
            opt(preceded(step_tag, numeric_expression)),
        )),
        |(control_variable, initial_value, limit, increment)| ast::ForStatement {
            control_variable,
            initial_value,
            limit,
            increment,
        },
    )(i)
}

fn next_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::NextStatement, E> {
    let next_tag = terminated(tag("NEXT"), space1);
    map(
        preceded(next_tag, simple_numeric_variable),
        |control_variable| ast::NextStatement { control_variable },
    )(i)
}

// 14. PRINT statement

fn print_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let print_tag = tag("PRINT");
    map(
        preceded(print_tag, opt(preceded(space1, print_list))),
        |list| {
            ast::Statement::Print(ast::PrintStatement {
                list: list.unwrap_or_else(Vec::new),
            })
        },
    )(i)
}

fn print_list<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<ast::PrintItem>, E> {
    let items = many0(pair(opt(print_item), print_separator));
    let trailing_item = opt(print_item);
    map(pair(items, trailing_item), |(items, trailing_item)| {
        ast::new_print_items(items, trailing_item)
    })(i)
}

fn print_item<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::PrintItem, E> {
    alt((
        // Note: expression consumes T of TAB, therefore tab_call needs to be parsed first.
        tab_call,
        map(expression, ast::PrintItem::Expression),
    ))(i)
}

fn tab_call<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::PrintItem, E> {
    map(
        preceded(tag("TAB("), terminated(numeric_expression, char(')'))),
        ast::PrintItem::TabCall,
    )(i)
}

fn print_separator<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::PrintItem, E> {
    let separator = alt((
        map(char(','), |_| ast::PrintItem::Comma),
        map(char(';'), |_| ast::PrintItem::Semicolon),
    ));
    preceded(space0, terminated(separator, space0))(i)
}

// 15. INPUT statement

fn input_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let input_tag = terminated(tag("INPUT"), space1);
    let separator = preceded(space0, terminated(char(','), space0));
    let variables = separated_nonempty_list(separator, variable);
    map(preceded(input_tag, variables), ast::Statement::Input)(i)
}

pub fn input_reply<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<ast::Datum>, E> {
    let padded_datum = preceded(space0, terminated(datum, space0));
    separated_nonempty_list(char(','), padded_datum)(i)
}

fn datum<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Datum, E> {
    // TODO: Avoid using owned string.
    alt((
        map(unquoted_string, |s| {
            ast::Datum::Unquoted(ast::StringConstant(s.to_string()))
        }),
        map(string_constant, ast::Datum::Quoted),
    ))(i)
}

// 16. READ and RESTORE statements

fn read_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let read_tag = terminated(tag("READ"), space1);
    let separator = preceded(space0, terminated(char(','), space0));
    let variables = separated_nonempty_list(separator, variable);
    map(preceded(read_tag, variables), ast::Statement::Read)(i)
}

fn restore_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Statement, E> {
    map(tag("RESTORE"), |_| ast::Statement::Restore)(i)
}

// 17. DATA statement

fn data_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let data_tag = terminated(tag("DATA"), space1);
    map(preceded(data_tag, data_list), ast::Statement::Data)(i)
}

fn data_list<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<ast::Datum>, E> {
    let separator = preceded(space0, terminated(char(','), space0));
    separated_nonempty_list(separator, datum)(i)
}

// 18. ARRAY declarations

fn dimension_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Statement, E> {
    let dim_tag = terminated(tag("DIM"), space1);
    let separator = preceded(space0, terminated(char(','), space0));
    let array_declarations = separated_nonempty_list(separator, array_declaration);
    map(preceded(dim_tag, array_declarations), ast::Statement::Dim)(i)
}

fn array_declaration<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::ArrayDeclaration, E> {
    map(
        pair(letter, preceded(char('('), terminated(bounds, char(')')))),
        |(letter, bounds)| ast::ArrayDeclaration { letter, bounds },
    )(i)
}

fn bounds<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (u64, Option<u64>), E> {
    pair(integer, opt(preceded(char(','), integer)))(i)
}

fn option_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    let option_tag = terminated(tag("OPTION"), space1);
    let base_tag = terminated(tag("BASE"), space1);
    let n = alt((
        map(char('0'), |_| ast::OptionBase::Base0),
        map(char('1'), |_| ast::OptionBase::Base1),
    ));
    map(
        preceded(option_tag, preceded(base_tag, n)),
        ast::Statement::OptionBase,
    )(i)
}

// 19. REMARK statement

fn remark_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(preceded(tag("REM"), remark_string), |_| ast::Statement::Rem)(i)
}

// 20. RANDOMIZE statement

fn randomize_statement<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, ast::Statement, E> {
    map(tag("RANDOMIZE"), |_| ast::Statement::Randomize)(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom5::error::VerboseError;

    #[test]
    fn test_numeric_constant() {
        let res = numeric_constant::<VerboseError<&str>>("-123456E-29");
        let (remaining, value) = res.expect("failed to parse");
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(value, ast::NumericConstant::from((-123456f64, -29)));
    }

    #[test]
    fn test_end_program() {
        let res = program::<VerboseError<&str>>("999 END");
        let (_, value) = res.expect("failed to parse");
        let program = value.expect("invalid program");
        assert_eq!(program.blocks.len(), 1);
    }

    #[test]
    fn test_variable_exampes() {
        let (_, v) = variable::<VerboseError<&str>>("X").expect("failed to parse");
        assert_eq!(
            v,
            ast::Variable::Numeric(ast::NumericVariable::Simple(ast::SimpleNumericVariable {
                letter: 'X',
                digit: None,
            })),
        );

        let (_, v) = variable::<VerboseError<&str>>("A5").expect("failed to parse");
        assert_eq!(
            v,
            ast::Variable::Numeric(ast::NumericVariable::Simple(ast::SimpleNumericVariable {
                letter: 'A',
                digit: Some(5),
            })),
        );

        let (_, v) = variable::<VerboseError<&str>>("S$").expect("failed to parse");
        assert_eq!(v, ast::Variable::String(ast::StringVariable('S')));

        let (_, v) = variable::<VerboseError<&str>>("C$").expect("failed to parse");
        assert_eq!(v, ast::Variable::String(ast::StringVariable('C')));
    }

    #[test]
    fn test_expression_examples() {
        expression::<VerboseError<&str>>("3*X - Y^2").expect("failed to parse");
        expression::<VerboseError<&str>>("A(1)+A(2)+A(3)").expect("failed to parse");
        expression::<VerboseError<&str>>("2^(-X)").expect("failed to parse");
        expression::<VerboseError<&str>>("-X/Y").expect("failed to parse");
        expression::<VerboseError<&str>>("SQR(X^2+Y^2)").expect("failed to parse");
    }

    #[test]
    fn test_user_defined_function_examples() {
        def_statement::<VerboseError<&str>>("DEF FNF(X) = X^4 - 1").expect("failed to parse");
        def_statement::<VerboseError<&str>>("DEF FNP = 3.14159").expect("failed to parse");
        def_statement::<VerboseError<&str>>("DEF FNA(X) = A*X + B").expect("failed to parse");
    }

    #[test]
    fn test_let_statement_examples() {
        let_statement::<VerboseError<&str>>("LET P = 3.14159").expect("failed to parse");
        let_statement::<VerboseError<&str>>("LET A(X,3) = SIN(X)*Y + 1").expect("failed to parse");
        let_statement::<VerboseError<&str>>("LET A$ = \"ABC\"").expect("failed to parse");
        let_statement::<VerboseError<&str>>("LET A$ = B$").expect("failed to parse");
    }

    #[test]
    fn test_control_statement_examples() {
        goto_statement::<VerboseError<&str>>("GO TO 999").expect("failed to parse");
        if_then_statement::<VerboseError<&str>>("IF X > Y+83 THEN 200").expect("failed to parse");
        if_then_statement::<VerboseError<&str>>("IF A$ <> B$ THEN 550").expect("failed to parse");
        on_goto_statement::<VerboseError<&str>>("ON L+1 GO TO 300,400,500")
            .expect("failed to parse");
    }

    #[test]
    fn test_for_and_next_statement_examples() {
        for_block::<VerboseError<&str>>("100 FOR I = 1 TO 10\n200 NEXT I\n")
            .expect("failed to parse");
        for_block::<VerboseError<&str>>("100 FOR I = A TO B STEP -1\n200 NEXT I\n")
            .expect("failed to parse");
    }

    #[test]
    fn test_print_statement_examples() {
        print_statement::<VerboseError<&str>>("PRINT X").expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT X; (Y+Z)/2").expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT").expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT TAB(10); A$; \"IS DONE.\"")
            .expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT \"X EQUALS\", 10").expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT X, Y").expect("failed to parse");
        print_statement::<VerboseError<&str>>("PRINT ,,,X").expect("failed to parse");
    }

    #[test]
    fn test_input_statement_examples() {
        input_statement::<VerboseError<&str>>("INPUT X").expect("failed to parse");
        input_statement::<VerboseError<&str>>("INPUT X, A$, Y(2)").expect("failed to parse");
        input_statement::<VerboseError<&str>>("INPUT A, B, C").expect("failed to parse");
    }

    #[test]
    fn test_input_reply_examples() {
        input_reply::<VerboseError<&str>>("3.14159").expect("failed to parse");
        input_reply::<VerboseError<&str>>("2,SMITH,-3").expect("failed to parse");
        input_reply::<VerboseError<&str>>("25,0,-15").expect("failed to parse");
    }

    #[test]
    fn test_read_statement_examples() {
        read_statement::<VerboseError<&str>>("READ X, Y, Z").expect("failed to parse");
        read_statement::<VerboseError<&str>>("READ X(1), A$, C").expect("failed to parse");
    }

    #[test]
    fn test_data_statement_examples() {
        data_statement::<VerboseError<&str>>("DATA 3.14159, PI, 5E-10, \",\"")
            .expect("failed to parse");
    }

    #[test]
    fn test_dimension_statement_examples() {
        dimension_statement::<VerboseError<&str>>("DIM A(6), B(10, 10)").expect("failed to parse");
    }

    #[test]
    fn test_remark_statement_examples() {
        remark_statement::<VerboseError<&str>>("REMARK FINAL CHECK").expect("failed to parse");
    }

    #[test]
    fn test_randomize_statement_examples() {
        randomize_statement::<VerboseError<&str>>("RANDOMIZE").expect("failed to parse");
    }
}
