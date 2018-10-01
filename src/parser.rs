use ast::*;
use error::Error;

use nom::types::CompleteStr;
use nom::{eol, space, space0, IResult};
use nom_locate::LocatedSpan;

use std::fmt;
use std::num;

/// Captures the offset, line number and remaining text for the current position of the parser.
pub type Span<'a> = LocatedSpan<CompleteStr<'a>>;

/// Parser error codes.
///
/// We use a nom's simple error flavour, which only allows to return error codes
/// when parser fails.
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

named!(letter<Span, char>,
    one_of!("ABCDEFGHIJKLMNOPQRSTUVWXYZ"));

named!(digit<Span, u8>,
    map!(one_of!("0123456789"), |c| c as u8 - b'0'));

// 5. Programs

named!(pub program<Span, Result<Program, Error>>,
    do_parse!(
        blocks: many0!(terminated!(block, end_of_line)) >>
        opt!(end_of_line) >>
        (Program::new(blocks))
    ));

named!(block<Span, Block>,
    do_parse!(
        line_number: line_number >>
        statement: sep!(space, statement) >>
        space0 >>
        (Block::Line{
            line_number,
            statement: statement.0,
            statement_source: statement.1
        })
    ));

named!(line_number<Span, u16>,
    map_res!(take_while_m_n!(1, 4, is_digit), from_decimal));

named!(end_of_line<Span, Span>, alt!(eof!() | eol));

named!(end_statement<Span, Statement>,
    do_parse!(
        tag!("END") >>
        (Statement::End)
    ));

named!(statement<Span, (Statement, Span)>,
    do_parse!(
        statement_source: position!() >>
        statement: alt!(
            goto_statement |
            gosub_statement |
            if_then_statement |
            on_goto_statement |
            let_statement |
            for_statement | 
            next_statement |
            print_statement |
            return_statement |
            stop_statement |
            read_statement |
            data_statement |
            remark_statement |
            end_statement) >>
        (statement, statement_source)
    ));

// 6. Constants

named!(numeric_constant<Span, (Sign, f64, i32)>,
    do_parse!(
        sign: opt!(sign) >>
        space0 >>
        numeric_rep: numeric_rep >>
        (sign.unwrap_or(Sign::Pos), numeric_rep.0, numeric_rep.1)
    ));

named!(sign<Span, Sign>,
    alt!(
        char!('+') => { |_| Sign::Pos } |
        char!('-') => { |_| Sign::Neg }
    ));

named!(numeric_rep<Span, (f64, i32)>,
    map!(pair!(significand, opt!(exrad)),
        |(significand, exrad)| (significand, exrad.unwrap_or(0))));

named!(significand<Span, f64>,
    alt!(
        pair!(opt!(integer), fraction) => {
            |(integral, frac): (Option<u64>, f64)| integral.unwrap_or(0) as f64 + frac
        }
        | pair!(integer, opt!(char!('.'))) => { |(i, _)| i as f64 }
    ));

named!(integer<Span, u64>,
    map_res!(take_while1!(is_digit), u64_from_decimal));

named!(fraction<Span, f64>,
    map_res!(preceded!(char!('.'), take_while1!(is_digit)),
        |s| u64_from_decimal(s).map(|frac| {
            frac as f64 / (10_u64.pow(s.fragment.len() as u32) as f64)
        })));

named!(exrad<Span, i32>,
    map!(
        preceded!(char!('E'),
            pair!(
                opt!(sign),
                map_res!(take_while1!(is_digit), i32_from_decimal)
            )
        ),
        |(sign, exp)| sign.unwrap_or(Sign::Pos) * exp
    ));

named!(string_constant<Span, StringConstant>,
    do_parse!(
        s: delimited!(tag!("\""), take_until!("\""), tag!("\"")) >>
        (StringConstant(s.to_string()))
    ));

// 7. Variables

named!(variable<Span, Variable>,
    alt!(
        do_parse!(
            var: numeric_variable >>
            (Variable::Numeric(var))
        ) |
        do_parse!(
            var: string_variable >>
            (Variable::String(var))
        )
    )
);

named!(numeric_variable<Span, NumericVariable>,
    alt!(simple_numeric_variable));

named!(simple_numeric_variable<Span, NumericVariable>,
    do_parse!(
        letter: letter >>
        digit: opt!(digit) >>
        (NumericVariable::Simple { letter, digit })
    ));

named!(string_variable<Span, StringVariable>,
    map!(terminated!(letter, char!('$')), StringVariable));

// 8. Expressions

named!(expression<Span, Expression>,
    alt!(
        // Note: Since numeric_expression sometimes matches a prefix of a string_expression,
        // first we need to try to parse string_expression, and only after it numeric_expression.
        map!(string_expression, Expression::String) |
        map!(numeric_expression, Expression::Numeric)
    ));

named!(string_expression<Span, StringExpression>,
    alt!(
        map!(string_variable, StringExpression::Variable) |
        map!(string_constant, StringExpression::Constant)
    ));

named!(numeric_expression<Span, NumericExpression>,
    do_parse!(
        leading_sign: opt!(sign) >>
        space0 >>
        leading_term: term >>
        terms: many0!(pair!(
            preceded!(space0, sign),
            preceded!(space0, term))) >>
        (NumericExpression::new(leading_sign, leading_term, terms))
    ));

named!(term<Span, Term>,
    do_parse!(
        leading_factor: factor >>
        factors: many0!(pair!(
            preceded!(space0, multiplier),
            preceded!(space0, factor))) >>
        (Term::new(leading_factor, factors))
    ));

named!(factor<Span, Factor>,
    do_parse!(
        leading_primary: primary >>
        primaries: many0!(preceded!(
            preceded!(space0, char!('^')),
            preceded!(space0, primary))) >>
        (Factor::new(leading_primary, primaries))
    ));

named!(multiplier<Span, Multiplier>,
    alt!(
        char!('*') => { |_| Multiplier::Mul } |
        char!('/') => { |_| Multiplier::Div }
    ));

named!(primary<Span, Primary>,
    alt!(
        map!(numeric_variable, Primary::Variable) |
        map!(numeric_rep, |(significand, exrad)| Primary::Constant(significand, exrad)) |
        map!(delimited!(char!('('), numeric_expression, char!(')')), Primary::Expression)
    ));

named!(relation<Span, Relation>,
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

named!(equality_relation<Span, EqualityRelation>,
    alt!(
        tag!("==") => { |_| EqualityRelation::EqualTo } |
        tag!("=") => { |_| EqualityRelation::EqualTo} |
        tag!("<>") => { |_| EqualityRelation::NotEqualTo }
    )
);

// 11. LET statement

named!(let_statement<Span, Statement>,
    map!(alt!(numeric_let_statement | string_let_statement), Statement::Let));

named!(numeric_let_statement<Span, LetStatement>,
    do_parse!(
        tag!("LET") >> space >>
        variable: numeric_variable >>
        space0 >>
        tag!("=") >>
        space0 >>
        expression: numeric_expression >>
        (LetStatement::Numeric{ variable, expression })
    ));

/// Finds position of constant in let statement when generating a warning.
pub fn let_statement_numeric_constant_pos<'a>(statement: &'a str) -> IResult<Span<'a>, Span<'a>> {
    named!(parse<Span, Span>,
        do_parse!(
            tag!("LET") >> space >>
            numeric_variable >>
            space0 >>
            tag!("=") >>
            space0 >>
            opt!(sign) >> // we need position after sign if there is any
            position: position!() >>
            numeric_expression >>
            (position)
        ));
    parse(Span::new(CompleteStr(statement)))
}

named!(string_let_statement<Span, LetStatement>,
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

named!(goto_statement<Span, Statement>,
    do_parse!(
        tag!("GO") >>
        space0 >>
        tag!("TO") >>
        space0 >>
        line_number: line_number >>
        (Statement::Goto(line_number))
    ));

named!(gosub_statement<Span, Statement>,
    do_parse!(
        tag!("GO") >>
        space0 >>
        tag!("SUB") >>
        space0 >>
        line_number: line_number >>
        (Statement::Gosub(line_number))
    ));

named!(if_then_statement<Span, Statement>,
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

named!(relational_expression<Span, RelationalExpression>,
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

named!(return_statement<Span, Statement>,
    do_parse!(
        tag!("RETURN") >>
        (Statement::Return)
    ));

named!(on_goto_statement<Span, Statement>, 
    do_parse!(
        tag!("ON") >>
        space0 >>
        numeric_expression: numeric_expression >> // looks like there's a typo in the ECMA spec
        space0 >>
        tag!("GO") >>
        space0 >>
        tag!("TO") >>
        space0 >>
        line_numbers: separated_nonempty_list!(char!(','), line_number) >>
        (Statement::OnGoto(OnGotoStatement { numeric_expression, line_numbers }))
));

named!(stop_statement<Span, Statement>,
    do_parse!(
        tag!("STOP") >>
        (Statement::Stop)
    ));

// 13. FOR and NEXT statements


named!(for_statement<Span, Statement>,
    do_parse!(
        tag!("FOR") >>
        space0 >>
        control_variable: simple_numeric_variable >>
        space0 >>
        tag!("=") >>
        space0 >>
        initial_value: numeric_expression >>
        space0 >>
        tag!("TO") >>
        space0 >>
        limit: numeric_expression >>
        space0 >>
        increment: opt!(step_increment) >>
        (Statement::For(
            ForStatement {
                control_variable, 
                initial_value, 
                limit, 
                increment: increment.unwrap_or(NumericExpression::with_constant(1.0))
            })
        )
));

named!(next_statement<Span, Statement>,
    do_parse!(
        tag!("NEXT") >>
        space0 >>
        control_variable: simple_numeric_variable >>
        (Statement::Next(control_variable))
    )
);

named!(step_increment<Span, NumericExpression>,
    do_parse!(
        tag!("STEP") >>
        space0 >>
        numeric_expression: numeric_expression >>
        (numeric_expression)
));

// 14. PRINT statement

named!(print_statement<Span, Statement>,
    do_parse!(
        tag!("PRINT") >>
        print_list: opt!(sep!(space, print_list)) >>
        (Statement::Print(PrintStatement{ list: print_list.unwrap_or_else(Vec::new) }))
    ));

named!(print_list<Span, Vec<PrintItem>>,
    do_parse!(
        items: many0!(pair!(opt!(print_item), print_separator)) >>
        trailing_item: opt!(print_item) >>
        (new_print_items(items, trailing_item))
    ));

named!(print_item<Span, PrintItem>,
    alt!(
        // Note: expression consumes T of TAB, therefore tab_call needs to be parsed first.
        tab_call |
        map!(expression, PrintItem::Expression)
    ));

named!(tab_call<Span, PrintItem>,
    map!(
        delimited!(tag!("TAB("), numeric_expression, char!(')')),
        PrintItem::TabCall
    ));

named!(print_separator<Span, PrintItem>,
    terminated!(preceded!(space0, alt!(
        char!(',') => { |_| PrintItem::Comma } |
        char!(';') => { |_| PrintItem::Semicolon }
    )), space0));

// 16. READ and RESTORE statements

named!(read_statement<Span, Statement>,
    do_parse!(
        tag!("READ") >>
        space0 >>
        variables: many1!(
            do_parse!(
                space0 >>
                opt!(char!(',')) >>
                space0 >>
                variable: variable >>
                (variable)
            )
        ) >>
        (Statement::Read(variables))
    )
);

named!(restore_statement<Span, Statement>,
    do_parse!(
        tag!("RESTORE") >>
        space0 >>
        (Statement::Restore)
    )
);

// 17. DATA statement

named!(data_statement<Span, Statement>, 
    do_parse!(
        tag!("DATA") >>
        space0 >>
        datum: many1!(
            do_parse!(
                space0 >>
                opt!(char!(',')) >>
                space0 >>
                expression: expression >>
                (expression)
            )
        ) >>
        (Statement::Data(datum))
    )
);

// 19. REMARK statement

named!(remark_statement<Span, Statement>,
    do_parse!(
        tag!("REM") >>
        space0 >>
        take_until!("\n") >>
        (Statement::Rem)
    ));

// helper

fn from_decimal(input: Span) -> Result<u16, num::ParseIntError> {
    u16::from_str_radix(&input.fragment, 10)
}

fn u64_from_decimal(input: Span) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(&input.fragment, 10)
}

fn i32_from_decimal(input: Span) -> Result<i32, num::ParseIntError> {
    i32::from_str_radix(&input.fragment, 10)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tab_call() {
        let res = tab_call(Span::new(CompleteStr("TAB(24)")));
        let (remaining, ast) = res.expect("failed to parse");
        let remaining = remaining.fragment;
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(
            ast,
            PrintItem::TabCall(NumericExpression::new(
                None,
                Term::new(Factor::new(Primary::Constant(24.0, 0), vec![]), vec![]),
                vec![]
            ))
        );
    }

    #[test]
    fn test_print_tab_call() {
        let res = print_statement(Span::new(CompleteStr("PRINT TAB(24)")));
        let (remaining, ast) = res.expect("failed to parse");
        let remaining = remaining.fragment;
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(
            ast,
            Statement::Print(PrintStatement {
                list: vec![PrintItem::TabCall(NumericExpression::new(
                    None,
                    Term::new(Factor::new(Primary::Constant(24.0, 0), vec![]), vec![]),
                    vec![],
                ))],
            })
        );
    }

    #[test]
    fn test_print_constant() {
        let res = print_statement(Span::new(CompleteStr(r#"PRINT " 0 ",0.000888," 0 ",-1.0"#)));
        let (remaining, ast) = res.expect("failed to parse");
        let remaining = remaining.fragment;
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
        let res = numeric_constant(Span::new(CompleteStr("-123456E-29")));
        let (remaining, float) = res.expect("failed to parse");
        let remaining = remaining.fragment;
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(float, (Sign::Neg, 123456f64, -29));
    }
}
