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
    ForWithoutNext,
    Unknown,
}

impl From<u32> for ErrorCode {
    fn from(code: u32) -> Self {
        match code {
            0 => ErrorCode::ExpectedStringExpression,
            1 => ErrorCode::ForWithoutNext,
            _ => ErrorCode::Unknown,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorCode::ExpectedStringExpression => f.write_str("string expression expected"),
            ErrorCode::ForWithoutNext => f.write_str("FOR without NEXT"),
            ErrorCode::Unknown => f.write_str("unknown"),
        }
    }
}

impl ErrorCode {
    /// Converts error code to BASIC conform error output.
    pub fn to_string(&self, span: &Span, input: &str) -> String {
        match self {
            ErrorCode::ExpectedStringExpression => {
                let line = input.lines().nth(span.line as usize - 1).unwrap();
                let mut parts = line.trim().splitn(2, ' ');
                let line_number = parts.next().unwrap();
                let statement = parts.next().unwrap();
                let remaining = span.fragment.lines().next().unwrap();
                let pos = statement.find(remaining).unwrap();
                format!(
                    "{}: error: {} \n {}\n{:width$}^\n",
                    line_number,
                    self,
                    statement,
                    "",
                    width = pos + 1
                )
            }
            ErrorCode::ForWithoutNext => {
                // span does not give a good context here, we need to find the first
                // FOR block by going through lines backwards
                let lines: Vec<_> = input.lines().collect();
                let for_line_str = lines[0..span.line as usize - 1]
                    .iter()
                    .rev()
                    .find(|l| l.find("FOR").is_some())
                    .unwrap();
                let (_, for_line) = for_line(Span::new(CompleteStr(for_line_str))).unwrap();
                format!("{}: error: {} \n", for_line.line_number, self)
            }
            ErrorCode::Unknown => panic!("bug in parser"),
        }
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

named!(unquoted_string_character<Span, char>,
    alt!(char!(' ') | plain_string_character));

named!(plain_string_character<Span, char>,
    alt!(
        char!('+') |
        char!('-') |
        char!('.') |
        one_of!("0123456789") |
        letter
    ));

named!(unquoted_string<Span, String>,
    map!(many1!(unquoted_string_character), {
        |chars: Vec<char>| chars.into_iter().collect::<String>()}));

// 5. Programs

pub fn program<'a>(input: Span<'a>) -> IResult<Span<'a>, Result<Program<'a>, Error>> {
    do_parse!(
        input,
        blocks: many0!(block)
            >> opt!(end_of_line)
            >> (Program::new(blocks, input.fragment.as_ref()))
    )
}

named!(block<Span, Block>,
    alt!(line | for_block));

named!(pub line<Span, Block>,
    do_parse!(
        line_number: line_number >>
        space >>
        statement_source: position!() >>
        statement: statement >>
        space0 >>
        end_of_line >>
        (Block::Line{
            line_number,
            statement,
            statement_source,
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

named!(statement<Span, Statement>,
    alt!(
        goto_statement |
        gosub_statement |
        on_goto_statement |
        if_then_statement |
        let_statement |
        print_statement |
        return_statement |
        stop_statement |
        read_statement |
        restore_statement |
        data_statement |
        remark_statement |
        end_statement
    ));

// 6. Constants

named!(pub numeric_constant<Span, NumericConstant>,
    do_parse!(
        sign: opt!(sign) >>
        space0 >>
        numeric_rep: numeric_rep >>
        (NumericConstant {
            sign: sign.unwrap_or(Sign::Pos),
            significand: numeric_rep.0,
            exrad: numeric_rep.1
        })
    ));

named!(sign<Span, Sign>,
    alt!(
        char!('+') => { |_| Sign::Pos } |
        char!('-') => { |_| Sign::Neg }
    ));

// We slightly deviate from the standard and allow an optional sign here:
// cf. test P038.
named!(numeric_rep<Span, (f64, i32)>,
    do_parse!(
        sign: opt!(sign) >>
        space0 >>
        significand: significand >>
        exrad: opt!(exrad) >>
        (sign.unwrap_or(Sign::Pos)  * significand, exrad.unwrap_or(0))
    ));

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
        s: delimited!(char!('"'), take_until!("\""), char!('"')) >>
        (StringConstant(s.to_string()))
    ));

// 7. Variables

named!(variable<Span, Variable>,
    alt!(
        map!(string_variable, Variable::String) |
        map!(numeric_variable, Variable::Numeric)
    ));

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
        map!(numeric_function_ref, Primary::Function) |
        map!(numeric_variable, Primary::Variable) |
        numeric_rep => { |value| Primary::Constant(NumericConstant::from(value)) } |
        map!(delimited!(char!('('), numeric_expression, char!(')')), Primary::Expression)
    ));

named!(numeric_function_ref<Span, Function>,
    alt!(
        pair!(tag!("ABS"), argument_list) => { |(_, arg)| Function::Abs(arg) } |
        pair!(tag!("ATN"), argument_list) => { |(_, arg)| Function::Atn(arg) } |
        pair!(tag!("COS"), argument_list) => { |(_, arg)| Function::Cos(arg) } |
        pair!(tag!("EXP"), argument_list) => { |(_, arg)| Function::Exp(arg) } |
        pair!(tag!("INT"), argument_list) => { |(_, arg)| Function::Int(arg) } |
        pair!(tag!("LOG"), argument_list) => { |(_, arg)| Function::Log(arg) } |
        tag!("RND") => { |_| Function::Rnd } |
        pair!(tag!("SGN"), argument_list) => { |(_, arg)| Function::Sgn(arg) } |
        pair!(tag!("SIN"), argument_list) => { |(_, arg)| Function::Sin(arg) } |
        pair!(tag!("SQR"), argument_list) => { |(_, arg)| Function::Sqr(arg) } |
        pair!(tag!("TAN"), argument_list) => { |(_, arg)| Function::Tan(arg) }
    ));

named!(argument_list<Span, NumericExpression>,
    delimited!(char!('('), numeric_expression, char!(')')));

named!(string_expression<Span, StringExpression>,
    alt!(
        map!(string_variable, StringExpression::Variable) |
        map!(string_constant, StringExpression::Constant)
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
        space >>
        numeric_expression: numeric_expression >>
        space >>
        tag!("GO") >>
        space0 >>
        tag!("TO") >>
        space >>
        line_numbers: separated_nonempty_list!(
            delimited!(space0, char!(','), space0), line_number) >>
        (Statement::OnGoto(OnGotoStatement { numeric_expression, line_numbers }))
    ));

named!(stop_statement<Span, Statement>,
    do_parse!(
        tag!("STOP") >>
        (Statement::Stop)
    ));

// 13. FOR and NEXT statements

named!(for_block<Span, Block>,
    do_parse!(
        for_line: for_line >>
        for_body: for_body >>
        (Block::For {
            for_line,
            blocks: for_body.0,
            next_line: for_body.1,
        })
    ));

named!(for_body<Span, (Vec<Block>, NextLine)>,
    do_parse!(
        blocks: many0!(block) >>
        next_line: return_error!(ErrorKind::Custom(ErrorCode::ForWithoutNext as u32), next_line) >>
        ((blocks, next_line))
    ));

named!(for_line<Span, ForLine>,
    do_parse!(
        line_number: line_number >>
        space >>
        statement_source: position!() >>
        for_statement: for_statement >>
        space0 >>
        end_of_line >>
        (ForLine{
            line_number,
            for_statement,
            statement_source,
        })
    ));

named!(pub next_line<Span, NextLine>,
    do_parse!(
        line_number: line_number >>
        space >>
        statement_source: position!() >>
        next_statement: next_statement >>
        space0 >>
        end_of_line >>
        (NextLine{
            line_number,
            next_statement,
            statement_source,
        })
    ));

named!(for_statement<Span, ForStatement>,
    do_parse!(
        tag!("FOR") >>
        space >>
        control_variable: simple_numeric_variable >>
        space0 >>
        char!('=') >>
        space0 >>
        initial_value: numeric_expression >>
        space >>
        tag!("TO") >>
        space >>
        limit: numeric_expression >>
        space0 >>
        opt!(tag!("STEP")) >>
        space0 >>
        increment: opt!(numeric_expression) >>
        space0 >>
        (ForStatement {
            control_variable,
            initial_value,
            limit,
            increment
        })
    ));

named!(next_statement<Span, NextStatement>,
    do_parse!(
        tag!("NEXT") >>
        space >>
        control_variable: simple_numeric_variable >>
        (NextStatement {
            control_variable
        })
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
        space >>
        variables: separated_nonempty_list!(char!(','), variable) >>
        (Statement::Read(variables))
    )
);

named!(restore_statement<Span, Statement>,
    map!(tag!("RESTORE"), |_| Statement::Restore));

// 17. DATA statement

named!(data_statement<Span, Statement>,
    do_parse!(
        tag!("DATA") >>
        space >>
        data: data_list >>
        (Statement::Data(data))
    ));

named!(data_list<Span, Vec<StringConstant>>,
    separated_nonempty_list!(
        delimited!(space0, char!(','), space0), datum));

named!(datum<Span, StringConstant>,
    alt!(map!(unquoted_string, StringConstant) | string_constant));

// 19. REMARK statement

named!(remark_statement<Span, Statement>,
    do_parse!(
        tag!("REM") >>
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
                Term::new(
                    Factor::new(Primary::Constant(NumericConstant::from((24.0, 0))), vec![]),
                    vec![]
                ),
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
                    Term::new(
                        Factor::new(Primary::Constant(NumericConstant::from((24.0, 0))), vec![]),
                        vec![]
                    ),
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
        let (remaining, value) = res.expect("failed to parse");
        let remaining = remaining.fragment;
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
        assert_eq!(value, NumericConstant::from((-123456f64, -29)));
    }

    #[test]
    fn test_data() {
        let res = data_statement(Span::new(CompleteStr("DATA 5,6,2D3")));
        let (remaining, _value) = res.expect("failed to parse");
        let remaining = remaining.fragment;
        assert!(remaining.is_empty(), "Remaing is not empty: {}", remaining);
    }

    #[test]
    fn test_for_block() -> Result<(), Error> {
        const PROGRAM: &str = r#"100 PRINT "Hello, World!"
110 FOR I = 0 TO 10 STEP 1
120 PRINT "Wow!"
121 PRINT "Wow!"
130 NEXT I
140 END
"#;

        let res = program(Span::new(CompleteStr(PROGRAM)))?;
        let (_, program) = res;
        let program = program?;
        // A FOR..NEXT-block is replaced by 7 equivalent lines + inner blocks
        assert_eq!(program.blocks.len(), 1 + 7 + 2 + 1);
        Ok(())
    }
}
