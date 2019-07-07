use crate::ast;
use crate::error;

use nom::types::CompleteStr;
use nom_locate::LocatedSpan;

use nom5::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1, take_while_m_n},
    character::complete::{char, one_of, space0, space1},
    combinator::{map, map_res, opt},
    error::{ErrorKind, ParseError},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};

// We use this constant to populate our ast which requires a span to be included.
// Later we should remove it in favor of proper nom 5 error handling.
const DUMMY_SPAN: LocatedSpan<CompleteStr<'static>> = LocatedSpan {
    offset: 0,
    line: 1,
    fragment: CompleteStr(""),
};

fn full_stop<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    char('.')(i)
}

fn space<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    char(' ')(i)
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

fn string_character<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    alt((quotation_mark, quoted_string_character))(i)
}

fn is_string_character(c: char) -> bool {
    c == '"' || is_quoted_string_character(c)
}

fn quoted_string_character<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    alt((one_of("!#$%&'()*,/:;<=>?^_"), unquoted_string_character))(i)
}

fn is_quoted_string_character(c: char) -> bool {
    match c {
        // quoted_string_character
        '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | ',' | '/' | ':' | ';' | '<'
        | '=' | '>' | '?' | '^' | '_' => true,
        other => is_unquoted_string_character(other),
    }
}

fn unquoted_string_character<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
    alt((space, plain_string_character))(i)
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
    take_while1(is_string_character)(i)
}

fn quoted_string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(
        quotation_mark,
        terminated(take_while(is_quoted_string_character), quotation_mark),
    )(i)
}

fn unquoted<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
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

fn program<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Result<ast::Program<'a>, error::Error>, E> {
    map(pair(many0(block), end_line), |(mut blocks, end_line)| {
        blocks.push(end_line);
        ast::Program::new(blocks, i)
    })(i)
}

// FIXME: This rule is different from the rule in the spec in the sense that a block consists of
// many lines and not a single one.
fn block<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block<'a>, E> {
    alt((line, for_block))(i)
}

fn line<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block<'a>, E> {
    map(
        tuple((
            terminated(line_number, space1),
            terminated(statement, space0),
            end_of_line,
        )),
        |(line_number, statement, _)| ast::Block::Line {
            line_number,
            statement,
            statement_source: DUMMY_SPAN,
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
            statement_source: DUMMY_SPAN,
        },
    )(i)
}

fn end_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(tag("END"), |_| ast::Statement::End)(i)
}

fn statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    remark_statement(i)
}

// 6. Constants

fn numeric_constant<'a, E: ParseError<&'a str>>(
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
        map(simple_numeric_variable, ast::NumericVariable::Simple),
        map(numeric_array_element, ast::NumericVariable::Array),
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
    // not implemented yet
    Err(nom5::Err::Error(E::from_error_kind(i, ErrorKind::Fix)))
    // let inner = pair(
    //     numeric_expression,
    //     opt(preceded(char(','), numeric_expression)),
    // );
    // preceded(char('('), terminated(inner, char(')')))(i)
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

// 13. FOR and NEXT statements

fn for_block<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Block, E> {
    // not implemented yet
    Err(nom5::Err::Error(E::from_error_kind(i, ErrorKind::Fix)))
}

// 19. REMARK statement

fn remark_statement<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ast::Statement, E> {
    map(terminated(tag("REM"), take_until("\n")), |_| {
        ast::Statement::Rem
    })(i)
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
    fn test_end_proram() {
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
        // expression::<VerboseError<&str>>("A(1)+A(2)+A(3)").expect("failed to parse");
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
}
