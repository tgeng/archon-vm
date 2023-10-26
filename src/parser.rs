use std::collections::HashMap;
use nom::branch::alt;
use nom::{InputLength, IResult};
use nom::character::complete::{line_ending, space0, char, satisfy as char_satisfy, alphanumeric1, one_of};
use nom::combinator::{cut, map, opt};
use nom::bytes::complete::{take_while1, escaped};
use nom::error::{context, ErrorKind, ParseError};
use nom::Parser;
use nom::multi::{many0, many0_count, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use crate::u_term::{Def, UTerm};
use nom_locate::{LocatedSpan};
use crate::parser::Fixity::{Infix, Prefix, Postfix, Infixl, Infixr};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy)]
enum Fixity {
    Infix,
    Infixl,
    Infixr,
    Prefix,
    Postfix,
}

type OperatorAndName = (&'static str, &'static str);

static PRECEDENCE: &[(&[OperatorAndName], Fixity)] = &[
    (&[("+", "_int_pos"), ("-", "_int_neg")], Prefix),
    (&[("*", "_int_mul"), ("/", "_int_div"), ("%", "_int_div")], Infixl),
    (&[("+", "_int_add"), ("-", "_int_sub")], Infixl),
    (&[(">", "_int_gt"), ("<", "_int_lt"), (">=", "_int_gte"), ("<=", "_int_lte"), ("==", "_int_eq"), ("!=", "_int_ne")], Infix),
    (&[("!", "_bool_not")], Prefix),
    (&[("&&", "_bool_and")], Infixl),
    (&[("||", "_bool_or")], Infixl),
];

// keywords
static KEYWORDS: &[&str] = &[
    "let", "def", "case_int", "case_str", "case_tuple", "force", "thunk", "=>", "="
];

// tokenizer

#[derive(Debug, Clone)]
enum Token {
    Normal(String, usize),
    Int(i64),
    Str(String),
    Indent(usize),
}

fn identifier_token(input: Span) -> IResult<Span, Token> {
    alt((
            // alpha-numeric identifier
            char_satisfy(|c| c.is_alphabetic() || c == '_')
                .and(take_while1(|c: char| c.is_alphanumeric() || c == '_'))
                .map(|(head, tail)| Token::Normal(format!("{}{}", head, tail), input.naive_get_utf8_column())),
            // specially handle some punctuations that should never be combined with others
            one_of("(),\\").map(|c| Token::Normal(c.to_string(), input.naive_get_utf8_column())),
            // punctuation identifier
            take_while1(|c: char| c.is_ascii_punctuation() && c != '`').map(|s: Span| Token::Normal(s.to_string(), input.naive_get_utf8_column())),
            // backtick-quoted identifier
            delimited(
                char('`'),
                take_while1(|c| c != '`'), char('`')).map(|s: Span| Token::Normal(s.to_string(), input.naive_get_utf8_column())),
        ),
    )(input)
}

fn int_token(input: Span) -> IResult<Span, Token> {
    map(
        take_while1(|c: char| c.is_ascii_digit()),
        |s: Span| Token::Int(s.to_string().parse::<i64>().unwrap()),
    )(input)
}

fn str_token(input: Span) -> IResult<Span, Token> {
    map(
        context(
            "string",
            preceded(
                char('\"'),
                cut(
                    terminated(
                        escaped(
                            alphanumeric1,
                            '\\',
                            one_of("\"\\")),
                        char('\"')))),
        ),
        |s: Span| Token::Str(s.to_string()),
    )(input)
}

fn indent_token(input: Span) -> IResult<Span, Token> {
    map(
        preceded(separated_list1(space0, line_ending), many0_count(char(' '))),
        Token::Indent,
    )(input)
}

fn tokens(input: Span) -> IResult<Span, Vec<Token>> {
    separated_list0(
        space0,
        alt((
            identifier_token,
            int_token,
            str_token,
            indent_token,
        )))(input)
}

// parser

#[derive(Debug, Clone)]
struct Input<'a> {
    tokens: &'a [Token],
    current_indent: usize,
}

impl<'a> InputLength for Input<'a> {
    fn input_len(&self) -> usize {
        self.tokens.input_len()
    }
}

fn map_token<F, R>(f: F) -> impl FnMut(Input) -> IResult<Input, R> where F: Fn(&Token) -> Option<R> {
    move |input: Input| {
        if input.tokens.is_empty() {
            Err(nom::Err::Incomplete(nom::Needed::Unknown))
        } else {
            let token = input.tokens.first().unwrap();
            match f(token) {
                Some(r) => Ok((Input { tokens: &input.tokens[1..], current_indent: input.current_indent }, r)),
                None => Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Satisfy })),
            }
        }
    }
}

fn satisfy<F>(f: F) -> impl FnMut(Input) -> IResult<Input, ()> where F: Fn(&Token) -> bool {
    move |input: Input| {
        if input.tokens.is_empty() {
            Err(nom::Err::Incomplete(nom::Needed::Unknown))
        } else {
            let token = input.tokens.first().unwrap();
            match f(token) {
                true => Ok((Input { tokens: &input.tokens[1..], current_indent: input.current_indent }, ())),
                false => Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Satisfy })),
            }
        }
    }
}

/// Update the current indent level according to the next token, which is assumed to be an
/// identifier.
fn scoped<'a, F, R>(mut f: F) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, R> where F: Parser<Input<'a>, R, nom::error::Error<Input<'a>>> {
    move |input| {
        if input.tokens.is_empty() {
            Err(nom::Err::Incomplete(nom::Needed::Unknown))
        } else {
            let token = input.tokens.first().unwrap();
            match token {
                Token::Normal(_, column) => match f.parse(Input { tokens: input.tokens, current_indent: *column }) {
                    Ok((new_input, r)) => Ok((Input { tokens: new_input.tokens, current_indent: input.current_indent }, r)),
                    Err(e) => Err(e)
                },
                _ => Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Satisfy }))
            }
        }
    }
}

fn newline(input: Input) -> IResult<Input, ()> {
    let current_indent = input.current_indent;
    // This local variable is needed to make rust's borrow checker happy. Otherwise it complains
    // `current_indent` does not live long enough.
    let mut p = satisfy(|token| matches!(token,Token::Indent(column) if { *column >= current_indent } ));
    p(input)
}

fn newline_opt(input: Input) -> IResult<Input, ()> {
    opt(newline)(input).map(|(input, _)| (input, ()))
}

fn token(s: &'static str) -> impl FnMut(Input) -> IResult<Input, ()> {
    satisfy(move |token| matches!(token, Token::Normal(name, _) if { name == s }))
}

fn id(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Normal(name, _) if { KEYWORDS.iter().all(|k| k != name) } => Some(name.clone()),
        _ => None,
    })(input)
}

fn id_term(input: Input) -> IResult<Input, UTerm> {
    map(id,
        |name| UTerm::Identifier { name },
    )(input)
}

fn int(input: Input) -> IResult<Input, i64> {
    map_token(|token| match token {
        Token::Int(value) => Some(*value),
        _ => None,
    })(input)
}

fn int_term(input: Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Int(value) => Some(UTerm::Int { value: *value }),
        _ => None,
    })(input)
}

fn str(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Str(value) => Some(value.clone()),
        _ => None,
    })(input)
}

fn str_term(input: Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Str(value) => Some(UTerm::Str { value: value.clone() }),
        _ => None,
    })(input)
}

fn empty_tuple(input: Input) -> IResult<Input, UTerm> {
    map(
        pair(token("("), token(")")),
        |_| UTerm::Tuple { values: vec![] },
    )(input)
}

fn singleton_tuple(input: Input) -> IResult<Input, UTerm> {
    map(
        delimited(
            pair(token("("), newline_opt),
            atom,
            pair(newline_opt, token(")"))),
        |t| UTerm::Tuple { values: vec![t] },
    )(input)
}

fn atom(input: Input) -> IResult<Input, UTerm> {
    alt((
        id_term,
        int_term,
        str_term,
        empty_tuple,
        singleton_tuple,
        delimited(
            pair(token("("), newline_opt),
            u_term,
            pair(newline_opt, token(")"))),
    ))(input)
}

fn scoped_app(input: Input) -> IResult<Input, UTerm> {
    scoped(
        map(
            separated_list1(newline, u_term),
            |parts| {
                let mut iter = parts.into_iter();
                let f = iter.next().unwrap();
                let args = iter.collect();
                UTerm::App { function: Box::new(f), args }
            },
        )
    )(input)
}

fn scoped_app_boxed() -> BoxedUTermParser {
    Box::new(move |input| scoped_app(input))
}

fn operator_id(operators: &'static [OperatorAndName]) -> impl FnMut(Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Normal(name, _) => {
            operators.iter()
                .filter_map(|(op, fun_name)|
                    if op == name {
                        Some(fun_name)
                    } else {
                        None
                    })
                .next()
                .map(|fun_name| UTerm::Identifier { name: fun_name.to_string() })
        }
        _ => None,
    })
}

pub fn infixl<I, O1, O2, E, F, G>(mut operator: F, mut operand: G) -> impl FnMut(I) -> IResult<I, (O2, Vec<(O1, O2)>), E>
    where
        I: Clone + InputLength,
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
{
    move |mut i: I| {
        let head = match operand.parse(i.clone()) {
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                let len = i.input_len();
                // infinite loop check: the parser must always consume
                if i1.input_len() == len {
                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                }
                i = i1;
                o
            }
        };
        let mut res = Vec::with_capacity(4);
        loop {
            let len = i.input_len();
            match operator.parse(i.clone()) {
                Err(nom::Err::Error(_)) => return Ok((i, (head, res))),
                Err(e) => return Err(e),
                Ok((i1, o1)) => {
                    // infinite loop check: the parser must always consume
                    if i1.input_len() == len {
                        return Err(nom::Err::Error(E::from_error_kind(i1, ErrorKind::Many1)));
                    }

                    match operand.parse(i1.clone()) {
                        Err(nom::Err::Error(_)) => return Ok((i, (head, res))),
                        Err(e) => return Err(e),
                        Ok((i2, o2)) => {
                            res.push((o1, o2));
                            i = i2;
                        }
                    }
                }
            }
        }
    }
}

pub fn infixr<I, O1, O2, E, F, G>(mut operator: F, mut operand: G) -> impl FnMut(I) -> IResult<I, (Vec<(O2, O1)>, O2), E>
    where
        I: Clone + InputLength,
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
{
    move |mut i: I| {
        let mut res = Vec::with_capacity(4);
        i = loop {
            let len = i.input_len();
            match operator.parse(i.clone()) {
                Err(nom::Err::Error(_)) => break i,
                Err(e) => return Err(e),
                Ok((i1, o1)) => {
                    // infinite loop check: the parser must always consume
                    if i1.input_len() == len {
                        return Err(nom::Err::Error(E::from_error_kind(i1, ErrorKind::Many1)));
                    }

                    match operand.parse(i1.clone()) {
                        Err(nom::Err::Error(_)) => break i,
                        Err(e) => return Err(e),
                        Ok((i2, o2)) => {
                            res.push((o2, o1));
                            i = i2;
                        }
                    }
                }
            }
        };
        match operand.parse(i.clone()) {
            Err(e) => Err(e),
            Ok((i1, o)) => {
                let len = i.input_len();
                // infinite loop check: the parser must always consume
                if i1.input_len() == len {
                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                }

                Ok((i1, (res, o)))
            }
        }
    }
}

pub fn infix<I, O1, O2, E, F, G>(mut operator: F, mut operand: G) -> impl FnMut(I) -> IResult<I, (O2, O1, O2), E>
    where
        I: Clone + InputLength,
        F: Parser<I, O1, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
{
    move |mut i: I| {
        let len = i.input_len();
        let first = match operand.parse(i.clone()) {
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                // infinite loop check: the parser must always consume
                if i1.input_len() == len {
                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                }

                i = i1;
                o
            }
        };
        let middle = match operator.parse(i.clone()) {
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                // infinite loop check: the parser must always consume
                if i1.input_len() == len {
                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                }

                i = i1;
                o
            }
        };
        let last = match operand.parse(i.clone()) {
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                // infinite loop check: the parser must always consume
                if i1.input_len() == len {
                    return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                }

                i = i1;
                o
            }
        };
        Ok((i, (first, middle, last)))
    }
}


type BoxedUTermParser = Box<dyn FnMut(Input) -> IResult<Input, UTerm>>;

fn operator_call(operators: &'static [OperatorAndName], fixity: Fixity, mut component: BoxedUTermParser) -> BoxedUTermParser {
    Box::new(
        move |input|
            match fixity {
                Infixl => map(
                    infixl(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(head, rest)| {
                        rest.into_iter().fold(head, |acc, (op, arg)| UTerm::App { function: Box::new(op), args: vec![acc, arg] })
                    },
                )(input),
                Infixr => map(
                    infixr(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(init, last)| {
                        init.into_iter().rfold(last, |acc, (arg, op)| UTerm::App { function: Box::new(op), args: vec![acc, arg] })
                    },
                )(input),
                Infix => map(
                    infix(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(first, middle, last)| UTerm::App { function: Box::new(middle), args: vec![first, last] },
                )(input),
                Prefix => map(
                    pair(operator_id(operators), |input| (*component)(input)),
                    |(operator, operand)| UTerm::App { function: Box::new(operator), args: vec![operand] },
                )(input),
                Postfix => map(
                    pair(|input| (*component)(input), operator_id(operators)),
                    |(operand, operator)| UTerm::App { function: Box::new(operator), args: vec![operand] },
                )(input),
            }
    )
}

fn expr_impl(input: Input) -> IResult<Input, UTerm> {
    let mut p = PRECEDENCE.iter().fold(scoped_app_boxed(), |f, (operators, fixity)| {
        operator_call(operators, *fixity, f)
    });
    (*p)(input)
}

fn expr(input: Input) -> IResult<Input, UTerm> {
    scoped(expr)(input)
}


fn tuple_or_term(input: Input) -> IResult<Input, UTerm> {
    map(
        separated_list1(
            delimited(newline_opt, token(","), newline_opt),
            expr),
        |values: Vec<UTerm>| {
            if values.len() == 1 {
                values.into_iter().next().unwrap()
            } else {
                UTerm::Tuple { values }
            }
        },
    )(input)
}

fn lambda(input: Input) -> IResult<Input, UTerm> {
    scoped(
        map(
            pair(
                delimited(
                    token("\\"),
                    separated_list0(newline_opt, id),
                    token("=>")),
                expr),
            |(arg_names, body)| UTerm::Lambda { arg_names, body: Box::new(body) },
        ))(input)
}

fn case_int(input: Input) -> IResult<Input, UTerm> {
    scoped(
        map(
            tuple((
                preceded(token("case_int"), map(expr, Box::new)),
                many0(map(scoped(tuple((preceded(newline, int), preceded(token("=>"), u_term)))), |(i, branch)| (i, branch))),
                preceded(newline, scoped(preceded(pair(token("_"), token("=>")), opt(map(u_term, Box::new))))),
            )),
            |(t, branch_entries, default_branch)| {
                let branches = HashMap::from_iter(branch_entries);
                UTerm::CaseInt { t, branches, default_branch }
            })
    )(input)
}

fn case_str(input: Input) -> IResult<Input, UTerm> {
    scoped(
        map(
            tuple((
                preceded(token("case_str"), map(expr, Box::new)),
                many0(map(scoped(tuple((preceded(newline, str), preceded(token("=>"), u_term)))), |(i, branch)| (i, branch))),
                preceded(newline, scoped(preceded(pair(token("_"), token("=>")), opt(map(u_term, Box::new))))),
            )),
            |(t, branch_entries, default_branch)| {
                let branches = HashMap::from_iter(branch_entries);
                UTerm::CaseStr { t, branches, default_branch }
            })
    )(input)
}

fn case_tuple(input: Input) -> IResult<Input, UTerm> {
    scoped(
        map(
            tuple((
                preceded(token("case_tuple"), map(expr, Box::new)),
                delimited(newline, many0(id), token("=>")),
                preceded(newline_opt, scoped(map(u_term, Box::new))),
            )),
            |(t, bound_names, branch)| {
                UTerm::CaseTuple { t, bound_names, branch }
            })
    )(input)
}

fn let_term(input: Input) -> IResult<Input, UTerm> {
    map(
        tuple((
            scoped(pair(delimited(token("let"), id, token("=")), u_term)),
            preceded(newline, u_term),
        )),
        |((name, t), body)| {
            UTerm::Let { name, t: Box::new(t), body: Box::new(body) }
        })(input)
}

fn defs_term(input: Input) -> IResult<Input, UTerm> {
    map(
        pair(
            many1(
                map(
                    scoped(
                        tuple((
                            preceded(token("def"), id),
                            many0(id),
                            preceded(token("=>"), map(u_term, Box::new))))),
                    |(name, args, block)| (name, Def { args, block }),
                )
            ), opt(preceded(newline, map(u_term, Box::new)))),
        |(defs, body)| UTerm::Defs { defs: HashMap::from_iter(defs), body },
    )(input)
}

fn u_term(input: Input) -> IResult<Input, UTerm> {
    alt((
        lambda,
        case_int,
        case_str,
        case_tuple,
        let_term,
        defs_term,
    ))(input)
}
