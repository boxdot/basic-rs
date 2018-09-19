use ast::*;
use error::Error;

use nom::types::CompleteStr;
use nom::{eol, space, space0};

use std::fmt;
use std::num;

pub enum ErrorCode {
    ExpectedStringExpression,
    Unknown,
}

impl From<u32> for ErrorCode {
    fn from(code: u32) -> Self {
        match code {
            0 => ErrorCode::ExpectedStringExpression,
            _ => ErrorCode::Unknown,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorCode::ExpectedStringExpression => f.write_str("string expression expected"),
            ErrorCode::Unknown => f.write_str("unknown"),
        }
    }
}

impl ErrorCode {
    /// Converts error code to BASIC conform error output.
    ///
    /// # Arguments
    ///
    /// `line` - Full line string where the error happened.
    /// `remaining` - Remaining statement which could not be parsed. Must be a substring of `line`.
    pub fn to_string(&self, line: &str, remaining: &str) -> Option<String> {
        let mut parts = line.trim().splitn(2, ' ');
        let line_number = parts.next()?;
        let statement = parts.next()?;
        let pos = statement.find(remaining)?;
        Some(format!(
            "{}: error: {} \n {}\n{:width$}^\n",
            line_number,
            self,
            statement,
            "",
            width = pos + 1
        ))
    }
}

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
    map!(one_of!("0123456789"), |c| c as u8 - b'0'));

// 5. Programs

named!(pub program<CompleteStr, Result<Program, Error>>,
    do_parse!(
        blocks: many0!(terminated!(block, end_of_line)) >>
        opt!(end_of_line) >>
        (Program::new(blocks))
    ));

named!(block<CompleteStr, Block>,
    do_parse!(
        line_number: line_number >>
        statement: sep!(space, statement) >>
        space0 >>
        (Block::Line{ line_number, statement })
    ));

named!(line_number<CompleteStr, u16>,
    map_res!(take_while_m_n!(1, 4, is_digit), from_decimal));

named!(end_of_line<CompleteStr, CompleteStr>, alt!(eof!() | eol));

named!(end_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("END") >>
        (Statement::End)
    ));

named!(statement<CompleteStr, Statement>,
    alt!(
        goto_statement |
        gosub_statement |
        if_then_statement |
        let_statement |
        print_statement |
        return_statement |
        stop_statement |
        remark_statement |
        end_statement
    ));

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
        |(significand, exrad)| significand * 10.0_f64.powi(exrad.unwrap_or(0))));

named!(significand<CompleteStr, f64>,
    alt!(
        pair!(opt!(integer), fraction) => {
            |(integral, frac): (Option<u64>, f64)| integral.unwrap_or(0) as f64 + frac
        }
        | pair!(integer, opt!(char!('.'))) => { |(i, _)| i as f64 }
    ));

named!(integer<CompleteStr, u64>,
    map_res!(take_while1!(is_digit), u64_from_decimal));

named!(fraction<CompleteStr, f64>,
    map_res!(preceded!(char!('.'), take_while1!(is_digit)),
        |s| u64_from_decimal(s).map(|frac| {
            frac as f64 / (10_u64.pow(s.len() as u32) as f64)
        })));

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
        map!(numeric_variable, Primary::Variable) |
        map!(numeric_rep, Primary::Constant)
        // map!(delimited!(char!('('), numeric_expression, char!(')')), Primary::Expression)
    ));

named!(relation<CompleteStr, Relation>,
    alt!(
        tag!("==") => { |_| Relation::EqualTo } |
        tag!("=") => { |_| Relation::EqualTo} |
        tag!("<>") => { |_| Relation::NotEqualTo } |
        tag!("<=") => { |_| Relation::LessThanOrEqualTo } |
        tag!(">=") => { |_| Relation::GreaterThanOrEqualTo } |
        tag!("<") => { |_| Relation::LessThan } |
        tag!(">") => { |_| Relation::GreaterThan }
    )
);

named!(equality_relation<CompleteStr, EqualityRelation>,
    alt!(
        tag!("==") => { |_| EqualityRelation::EqualTo } |
        tag!("=") => { |_| EqualityRelation::EqualTo} |
        tag!("<>") => { |_| EqualityRelation::NotEqualTo }
    )
);

// 11. LET statement

named!(let_statement<CompleteStr, Statement>,
    map!(alt!(numeric_let_statement | string_let_statement), Statement::Let));

named!(numeric_let_statement<CompleteStr, LetStatement>,
    do_parse!(
        tag!("LET") >> space >>
        variable: numeric_variable >>
        space0 >>
        tag!("=") >>
        space0 >>
        expression: numeric_expression >>
        (LetStatement::Numeric{ variable, expression })
    ));

named!(string_let_statement<CompleteStr, LetStatement>,
    do_parse!(
        tag!("LET") >> space >>
        variable: string_variable >>
        space0 >>
        tag!("=") >>
        space0 >>
        expression: string_expression >>
        (LetStatement::String{ variable, expression })
    ));

// 12. Control statements

named!(goto_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("GO") >>
        space0 >>
        tag!("TO") >>
        space0 >>
        line_number: line_number >>
        (Statement::Goto(line_number))
    ));

named!(gosub_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("GO") >>
        space0 >>
        tag!("SUB") >>
        space0 >>
        line_number: line_number >>
        (Statement::Gosub(line_number))
    ));

named!(if_then_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("IF") >>
        space0 >>
        if_statement: relational_expression >>
        space0 >>
        tag!("THEN") >>
        space0 >>
        line_number: line_number >>
        (Statement::IfThen(if_statement, line_number))
    ));

named!(relational_expression<CompleteStr, RelationalExpression>,
    alt!(
        do_parse!(
        left_expression: numeric_expression >>
        space0 >>
        relation: relation >>
        space0 >>
        right_expression: numeric_expression >>
        (RelationalExpression::NumericComparison(left_expression, relation, right_expression))
    ) |
    do_parse!(
        left_expression: string_expression >>
        space0 >>
        relation: equality_relation >>
        space0 >>
        right_expression: return_error!(ErrorKind::Custom(
            ErrorCode::ExpectedStringExpression as u32), string_expression) >>
        (RelationalExpression::StringComparison(left_expression, relation, right_expression))
    ))
);

named!(return_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("RETURN") >>
        (Statement::Return)
    ));

named!(stop_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("STOP") >>
        (Statement::Stop)
    ));

// 14. PRINT statement

named!(print_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("PRINT") >>
        print_list: opt!(sep!(space, print_list)) >>
        (Statement::Print(PrintStatement{ list: print_list.unwrap_or_else(Vec::new) }))
    ));

named!(print_list<CompleteStr, Vec<PrintItem>>,
    do_parse!(
        items: many0!(pair!(opt!(print_item), print_separator)) >>
        trailing_item: opt!(print_item) >>
        (new_print_items(items, trailing_item))
    ));

named!(print_item<CompleteStr, PrintItem>,
    alt!(
        // Note: expression consumes T of TAB, therefore tab_call needs to be parsed first.
        tab_call |
        map!(expression, PrintItem::Expression)
    ));

named!(tab_call<CompleteStr, PrintItem>,
    map!(
        delimited!(tag!("TAB("), numeric_expression, char!(')')),
        PrintItem::TabCall
    ));

named!(print_separator<CompleteStr, PrintItem>,
    terminated!(preceded!(space0, alt!(
        char!(',') => { |_| PrintItem::Comma } |
        char!(';') => { |_| PrintItem::Semicolon }
    )), space0));

// 19. REMARK statement

named!(remark_statement<CompleteStr, Statement>,
    do_parse!(
        tag!("REM") >>
        space0 >>
        take_until!("\n") >>
        (Statement::Rem)
    ));

// helper

fn from_decimal(input: CompleteStr) -> Result<u16, num::ParseIntError> {
    u16::from_str_radix(&input, 10)
}

fn u64_from_decimal(input: CompleteStr) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(&input, 10)
}

fn i32_from_decimal(input: CompleteStr) -> Result<i32, num::ParseIntError> {
    i32::from_str_radix(&input, 10)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tab_call() {
        let res = tab_call(CompleteStr("TAB(24)"));
        let (remaining, ast) = res.expect("failed to parse");
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(
            ast,
            PrintItem::TabCall(NumericExpression::new(
                None,
                Term::new(Factor::new(Primary::Constant(24.0), vec![]), vec![]),
                vec![]
            ))
        );
    }

    #[test]
    fn test_print_tab_call() {
        let res = print_statement(CompleteStr("PRINT TAB(24)"));
        let (remaining, ast) = res.expect("failed to parse");
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(
            ast,
            Statement::Print(PrintStatement {
                list: vec![PrintItem::TabCall(NumericExpression::new(
                    None,
                    Term::new(Factor::new(Primary::Constant(24.0), vec![]), vec![]),
                    vec![],
                ))],
            })
        );
    }

    #[test]
    fn test_print_constant() {
        let res = print_statement(CompleteStr(r#"PRINT " 0 ",0.000888," 0 ",-1.0"#));
        let (remaining, ast) = res.expect("failed to parse");
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(
            ast,
            Statement::Print(PrintStatement {
                list: vec![
                    PrintItem::Expression(Expression::String(StringExpression::Constant(
                        StringConstant(" 0 ".into()),
                    ))),
                    PrintItem::Comma,
                    PrintItem::Expression(Expression::Numeric(NumericExpression::with_constant(
                        0.000888,
                    ))),
                    PrintItem::Comma,
                    PrintItem::Expression(Expression::String(StringExpression::Constant(
                        StringConstant(" 0 ".into()),
                    ))),
                    PrintItem::Comma,
                    PrintItem::Expression(Expression::Numeric(NumericExpression::with_constant(
                        -1.0,
                    ))),
                ],
            })
        );
    }

    #[test]
    fn test_numeric_constant() {
        let res = numeric_constant(CompleteStr("-123456E-29"));
        let (remaining, float) = res.expect("failed to parse");
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(float, -123456E-29);
    }
}
