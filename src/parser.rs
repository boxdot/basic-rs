use ast::*;

use nom::types::CompleteStr;
use nom::{eol, space};

use std::num;

fn is_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn from_decimal(input: CompleteStr) -> Result<u16, num::ParseIntError> {
    u16::from_str_radix(&input, 10)
}

named!(pub line_number<CompleteStr, u16>,
    map_res!(take_while_m_n!(1, 4, is_digit), from_decimal));

named!(string_constant<CompleteStr, StringConstant>,
    do_parse!(
        s: delimited!(tag!("\""), take_until!("\""), tag!("\"")) >>
        (StringConstant(s.to_string()))
    ));

named!(print_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("PRINT") >>
        string_constants: many0!(sep!(space, string_constant)) >>
        (Statement::Print(PrintStatement{ list: string_constants.into_iter().map(
            |s| PrintItem::Expression(
                Expression::String(StringExpression::Constant(s)))
            ).collect() }))
    ));

named!(statement<CompleteStr, Statement>,
    do_parse!(
        statement: print_statement >>
        (statement)));

named!(pub block<CompleteStr, Block>,
    do_parse!(
        line_number: line_number >>
        statement: sep!(space, statement) >>
        opt!(space) >>
        (Block::Line{ line_number, statement })
    ));

named!(end_of_line<CompleteStr, CompleteStr>, alt!(eof!() | eol));

named!(end_statement<CompleteStr, CompleteStr>,
    tag!("END"));

named!(end_line<CompleteStr, Statement>,
    do_parse!(
        line_number >>
        sep!(space, end_statement) >>
        opt!(space) >>
        (Statement::End)
    ));

named!(pub program<CompleteStr, Program>,
    do_parse!(
        blocks: many0!(terminated!(block, end_of_line)) >>
        end_line >>
        opt!(end_of_line) >>
        (Program{ blocks })
    ));
