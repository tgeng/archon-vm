use std::thread::scope;
use std::collections::HashMap;
use nom::branch::alt;
use nom::{InputLength, IResult};
use nom::character::complete::{line_ending, space0, char, satisfy as char_satisfy, alphanumeric1, one_of, anychar};
use nom::combinator::{cut, map, opt, peek, verify};
use nom::bytes::complete::{take_while1, escaped, tag};
use nom::error::{context};
use nom::Parser;
use nom::multi::{many0, many0_count, many1, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use crate::u_term::{Def, UTerm};
use nom_locate::{LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

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
                None => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::Satisfy })),
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
                false => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::Satisfy })),
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
                _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::Satisfy }))
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

fn expr(input: Input) -> IResult<Input, UTerm> {
    scoped(
        scoped_app
    )(input)
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
