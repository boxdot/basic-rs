use crate::ast;

use nom5::{
    branch::alt,
    bytes::complete::{take_while, take_while1, take_while_m_n},
    character::complete::{char, one_of},
    combinator::{map, map_res, opt},
    error::ParseError,
    sequence::{pair, preceded, terminated, tuple},
    IResult,
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

fn string_constant<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    quoted_string(i)
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
}
