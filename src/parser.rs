use ast::*;
use error::Error;

use nom::types::CompleteStr;
use nom::{eol, space};

use std::num;

// 4. Syntax

fn is_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

named!(letter<CompleteStr, char>,
    one_of!("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));

named!(digit<CompleteStr, u8>,
    map!(one_of!("0123456789"), |c| c as u8 - '0' as u8));

// 6. Constants

named!(numeric_constant<CompleteStr, f64>,
    do_parse!(
        sign: opt!(sign) >>
        numeric_rep: numeric_rep >>
        (sign.unwrap_or(Sign::Pos) * numeric_rep)
    ));

named!(sign<CompleteStr, Sign>,
    alt!(
        char!('+') => { |_| Sign::Pos } |
        char!('-') => { |_| Sign::Neg }
    ));

named!(numeric_rep<CompleteStr, f64>,
    map!(pair!(significand, opt!(exrad)),
        |(significand, exrad)| significand.powi(exrad.unwrap_or(1))));

named!(significand<CompleteStr, f64>,
    alt!(
        pair!(integer, opt!(char!('.'))) => { |(i, _)| i as f64 } |
        pair!(opt!(integer), fraction) => {
            |(integral, frac): (Option<u64>, u64)| u64_from_parts(integral.unwrap_or(0), frac)
        }
    ));

named!(integer<CompleteStr, u64>,
    map_res!(take_while1!(is_digit), u64_from_decimal));

named!(fraction<CompleteStr, u64>,
    map_res!(preceded!(char!('.'), take_while1!(is_digit)), u64_from_decimal));

named!(exrad<CompleteStr, i32>,
    map!(
        preceded!(char!('E'),
            pair!(
                opt!(sign),
                map_res!(take_while1!(is_digit), i32_from_decimal)
            )
        ),
        |(sign, exp)| sign.unwrap_or(Sign::Pos) * exp
    ));

named!(string_constant<CompleteStr, StringConstant>,
    do_parse!(
        s: delimited!(tag!("\""), take_until!("\""), tag!("\"")) >>
        (StringConstant(s.to_string()))
    ));

// 7. Variables

named!(numeric_variable<CompleteStr, NumericVariable>,
    alt!(simple_numeric_variable));

named!(simple_numeric_variable<CompleteStr, NumericVariable>,
    do_parse!(
        letter: letter >>
        digit: opt!(digit) >>
        (NumericVariable::Simple { letter, digit })
    ));

named!(string_variable<CompleteStr, StringVariable>,
    map!(terminated!(letter, char!('$')), StringVariable));

// 8. Expressions

named!(expression<CompleteStr, Expression>,
    alt!(
        // Note: Since numeric_expression sometimes matches a prefix of a string_expression,
        // first we need to try to parse string_expression, and only after it numeric_expression.
        map!(string_expression, Expression::String) |
        map!(numeric_expression, Expression::Numeric)
    ));

named!(string_expression<CompleteStr, StringExpression>,
    alt!(
        map!(string_variable, StringExpression::Variable) |
        map!(string_constant, StringExpression::Constant)
    ));

named!(numeric_expression<CompleteStr, NumericExpression>,
    do_parse!(
        leading_sign: opt!(sign) >>
        leading_term: term >>
        terms: many0!(pair!(sign, term)) >>
        (NumericExpression::new(leading_sign, leading_term, terms))
    ));

named!(term<CompleteStr, Term>,
    do_parse!(
        leading_factor: factor >>
        factors: many0!(pair!(multiplier, factor)) >>
        (Term::new(leading_factor, factors))
    ));

named!(factor<CompleteStr, Factor>,
    do_parse!(
        leading_primary: primary >>
        primaries: many0!(preceded!(char!('^'), primary)) >>
        (Factor::new(leading_primary, primaries))
    ));

named!(multiplier<CompleteStr, Multiplier>,
    alt!(
        char!('*') => { |_| Multiplier::Mul } |
        char!('/') => { |_| Multiplier::Div }
    ));

named!(primary<CompleteStr, Primary>,
    alt!(
        // map!(numeric_variable, Primary::Variable) |
        map!(numeric_rep, Primary::Constant)
        // map!(delimited!(char!('('), numeric_expression, char!(')')), Primary::Expression)
    ));

// 11. LET statement

named!(let_statement<CompleteStr, Statement>,
    map!(alt!(numeric_let_statement | string_let_statement), Statement::Let));

named!(numeric_let_statement<CompleteStr, LetStatement>,
    do_parse!(
        tag!("LET") >> space >>
        variable: numeric_variable >>
        opt!(space) >>
        tag!("=") >>
        opt!(space) >>
        expression: numeric_expression >>
        (LetStatement::Numeric{ variable, expression })
    ));

named!(string_let_statement<CompleteStr, LetStatement>,
    do_parse!(
        tag!("LET") >> space >>
        variable: string_variable >>
        opt!(space) >>
        tag!("=") >>
        opt!(space) >>
        expression: string_expression >>
        (LetStatement::String{ variable, expression })
    ));

// 14. PRINT statement

named!(tab_call<CompleteStr, PrintItem>,
    map!(
        delimited!(tag!("TAB("), numeric_expression, char!(')')),
        PrintItem::TabCall
    ));

named!(print_item<CompleteStr, PrintItem>,
    alt!(
        map!(expression, PrintItem::Expression) |
        tab_call
    ));

named!(print_item_comma<CompleteStr, PrintItem>,
    map!(char!(','), |_| PrintItem::Comma));

named!(print_item_semicolon<CompleteStr, PrintItem>,
    map!(char!(';'), |_| PrintItem::Semicolon));

named!(print_separator<CompleteStr, PrintItem>,
    do_parse!(
        many0!(space) >>
        sep: alt!(print_item_comma | print_item_semicolon) >>
        (sep)
    ));

named!(print_list<CompleteStr, Vec<PrintItem>>,
    do_parse!(
        items: many0!(pair!(opt!(print_item), print_separator)) >>
        trailing_item: opt!(print_item) >>
        (new_print_items(items, trailing_item))
    ));

named!(print_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("PRINT") >>
        print_list: opt!(sep!(space, print_list)) >>
        (Statement::Print(PrintStatement{ list: print_list.unwrap_or_else(Vec::new) }))
    ));

// Program

fn from_decimal(input: CompleteStr) -> Result<u16, num::ParseIntError> {
    u16::from_str_radix(&input, 10)
}

fn u64_from_decimal(input: CompleteStr) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(&input, 10)
}

fn i32_from_decimal(input: CompleteStr) -> Result<i32, num::ParseIntError> {
    i32::from_str_radix(&input, 10)
}

fn u64_from_parts(integral: u64, fractional: u64) -> f64 {
    let mut fractional = fractional as f64;
    while fractional >= 1.0 {
        fractional /= 10.;
    }
    integral as f64 + fractional
}

named!(line_number<CompleteStr, u16>,
    map_res!(take_while_m_n!(1, 4, is_digit), from_decimal));

named!(stop_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("STOP") >>
        (Statement::Stop)
    ));

named!(end_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("END") >>
        (Statement::End)
    ));

named!(statement<CompleteStr, Statement>,
    do_parse!(
        statement: alt!(print_statement | let_statement | stop_statement | end_statement) >>
        (statement)));

named!(pub block<CompleteStr, Block>,
    do_parse!(
        line_number: line_number >>
        statement: sep!(space, statement) >>
        opt!(space) >>
        (Block::Line{ line_number, statement })
    ));

named!(end_of_line<CompleteStr, CompleteStr>, alt!(eof!() | eol));

named!(pub program<CompleteStr, Result<Program, Error>>,
    do_parse!(
        blocks: many0!(terminated!(block, end_of_line)) >>
        opt!(end_of_line) >>
        (Program::new(blocks))
    ));
