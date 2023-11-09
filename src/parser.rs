use std::collections::HashMap;
use nom::branch::alt;
use nom::{InputLength, IResult};
use nom::character::complete::{space0, char, satisfy as char_satisfy, alphanumeric1, one_of};
use nom::combinator::{cut, map, map_res, opt};
use nom::bytes::complete::{take_while1, escaped};
use nom::error::{context, ErrorKind, ParseError};
use nom::Parser;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use crate::u_term::{Def, UTerm};
use nom_locate::{LocatedSpan};
use crate::parser::Fixity::{*};

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
    (&[("*", "_int_mul"), ("/", "_int_div"), ("%", "_int_mod")], Infixl),
    (&[("+", "_int_add"), ("-", "_int_sub")], Infixl),
    (&[(">", "_int_gt"), ("<", "_int_lt"), (">=", "_int_gte"), ("<=", "_int_lte"), ("==", "_int_eq"), ("!=", "_int_ne")], Infix),
    (&[("!", "_bool_not")], Prefix),
    (&[("&&", "_bool_and")], Infixl),
    (&[("||", "_bool_or")], Infixl),
];

// keywords
static KEYWORDS: &[&str] = &[
    "let", "def", "case_int", "case_str", "force", "thunk", "=>", "=", "(", ")", ",", "\\", "{", "}", "@", "_"
];

// tokenizer

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Normal(String, usize),
    Int(i64, usize),
    Str(String, usize),
    Indent(usize),
}

impl Token {
    fn column(&self) -> usize {
        match self {
            Token::Normal(_, column) => *column,
            Token::Int(_, column) => *column,
            Token::Str(_, column) => *column,
            Token::Indent(column) => *column,
        }
    }
}

fn identifier_token(input: Span) -> IResult<Span, Token> {
    context(
        "identifier_token",
        alt((
                // alpha-numeric identifier
                char_satisfy(|c| c.is_alphabetic() || c == '_')
                    .and(opt(take_while1(|c: char| c.is_alphanumeric() || c == '_')))
                    .map(|(head, tail)| {
                        let id_string = match tail {
                            Some(tail) => format!("{}{}", head, tail),
                            None => head.to_string(),
                        };
                        Token::Normal(id_string, input.naive_get_utf8_column() - 1)
                    }),
                // specially handle some punctuations that should never be combined with others
                one_of("(),\\{}").map(|c| Token::Normal(c.to_string(), input.naive_get_utf8_column() - 1)),
                // punctuation identifier
                take_while1(|c: char| c.is_ascii_punctuation() &&
                    c != '`' && c != '"' && c != '(' && c != ')' && c != ',' && c != '\\')
                    .map(|s: Span| Token::Normal(s.to_string(), input.naive_get_utf8_column() - 1)),
                // backtick-quoted identifier
                delimited(
                    char('`'),
                    take_while1(|c| c != '`'), char('`')).map(|s: Span| Token::Normal(s.to_string(), input.naive_get_utf8_column() - 1)),
            ),
        ))(input)
}

fn int_token(input: Span) -> IResult<Span, Token> {
    context(
        "int_token",
        map(
            take_while1(|c: char| c.is_ascii_digit()),
            |s: Span| Token::Int(s.to_string().parse::<i64>().unwrap(), s.naive_get_utf8_column() - 1),
        ))(input)
}

fn str_token(input: Span) -> IResult<Span, Token> {
    context(
        "str_token",
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
            |s: Span| Token::Str(s.to_string(), s.naive_get_utf8_column() - 2), // offset quote
        ))(input)
}

fn indent_token(input: Span) -> IResult<Span, Token> {
    context(
        "indent_token",
        map_res(
            many0(one_of(" \t\n\r")),
            |whitespaces| {
                let mut indent = 0;
                let mut has_newline = false;
                for c in whitespaces {
                    match c {
                        ' ' => indent += 1,
                        '\t' => indent += 4,
                        '\n' => {
                            indent = 0;
                            has_newline = true;
                        }
                        '\r' => indent = 0,
                        _ => panic!("unexpected whitespace character: {}", c),
                    }
                }
                if has_newline {
                    Ok(Token::Indent(indent))
                } else {
                    Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Space }))
                }
            },
        ))(input)
}

fn tokens(input: Span) -> IResult<Span, Vec<Token>> {
    preceded(
        many0(one_of(" \t\n\r")),
        separated_list0(
            space0,
            alt((
                identifier_token,
                int_token,
                str_token,
                indent_token,
            ))),
    )(input)
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
            Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Eof }))
        } else {
            let token = input.tokens.first().unwrap();
            match f(token) {
                Some(r) => Ok((Input { tokens: &input.tokens[1..], current_indent: input.current_indent }, r)),
                None => Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::MapRes })),
            }
        }
    }
}

fn satisfy<F>(f: F) -> impl FnMut(Input) -> IResult<Input, ()> where F: Fn(&Token) -> bool {
    move |input: Input| {
        if input.tokens.is_empty() {
            Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Eof }))
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
            Err(nom::Err::Error(nom::error::Error { input, code: ErrorKind::Eof }))
        } else {
            let token = input.tokens.first().unwrap();
            match f.parse(Input { tokens: input.tokens, current_indent: token.column() + 1 }) {
                Ok((new_input, r)) => Ok((Input { tokens: new_input.tokens, current_indent: input.current_indent }, r)),
                Err(e) => Err(e),
            }
        }
    }
}

fn newline(input: Input) -> IResult<Input, ()> {
    let current_indent = input.current_indent;
    // This local variable is needed to make rust's borrow checker happy. Otherwise it complains
    // `current_indent` does not live long enough.
    let mut p = satisfy(|token| matches!(token,Token::Indent(column) if *column >= current_indent ));
    p(input)
}

fn newline_opt(input: Input) -> IResult<Input, ()> {
    opt(newline)(input).map(|(input, _)| (input, ()))
}

fn token(s: &'static str) -> impl FnMut(Input) -> IResult<Input, ()> {
    move |input| {
        context(
            s,
            satisfy(move |token| matches!(token, Token::Normal(name, _) if { name == s })),
        )(input)
    }
}

fn id(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Normal(name, _)
        if KEYWORDS.iter().all(|k| k != name) &&
            PRECEDENCE.iter().all(|(names, ..)| names.iter().all(|(n, _)| n != name))
        => Some(name.clone()),
        _ => None,
    })(input)
}

fn id_term(input: Input) -> IResult<Input, UTerm> {
    context("id_term", map(id, |name| UTerm::Identifier { name }))(input)
}

fn int(input: Input) -> IResult<Input, i64> {
    map_token(|token| match token {
        Token::Int(value, _) => Some(*value),
        _ => None,
    })(input)
}

fn int_term(input: Input) -> IResult<Input, UTerm> {
    context("int_term", map(int, |value| UTerm::Int { value }))(input)
}

fn str(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Str(value, _) => Some(value.clone()),
        _ => None,
    })(input)
}

fn str_term(input: Input) -> IResult<Input, UTerm> {
    context("str_term", map(str, |value| UTerm::Str { value }))(input)
}

fn struct_(input: Input) -> IResult<Input, UTerm> {
    context("struct", map(
        delimited(
            token("{"),
            separated_list0(
                delimited(newline_opt, token(","), newline_opt),
                expr),
            token("}"),
        ),
        |values: Vec<UTerm>| UTerm::Struct { values },
    ))(input)
}

fn atom(input: Input) -> IResult<Input, UTerm> {
    context("atom", alt((
        id_term,
        int_term,
        str_term,
        struct_,
        delimited(
            pair(token("("), newline_opt),
            u_term,
            pair(newline_opt, token(")"))),
    )))(input)
}

fn force(input: Input) -> IResult<Input, UTerm> {
    context("force", map(preceded(token("force"), atom), |t| UTerm::Force { thunk: Box::new(t) }))(input)
}

fn mem_access_or_atom(input: Input) -> IResult<Input, UTerm> {
    context("mem_access_or_atom", map(
        pair(atom, opt(pair(preceded(token("@"), atom), opt(preceded(token("="), u_term))))),
        |(t, index_and_value)| {
            match index_and_value {
                Some((index, None)) => UTerm::MemGet { base: Box::new(t), offset: Box::new(index) },
                Some((index, Some(value))) => UTerm::MemSet { base: Box::new(t), offset: Box::new(index), value: Box::new(value) },
                None => t,
            }
        },
    ))(input)
}

fn scoped_app(input: Input) -> IResult<Input, UTerm> {
    context("scoped_app", scoped(
        map(
            tuple((alt((mem_access_or_atom, force)), many0(atom), many0(preceded(newline, u_term)))),
            |(f, args, more_args)| {
                if args.is_empty() && more_args.is_empty() {
                    f
                } else {
                    let all_args = args.into_iter().chain(more_args).collect();
                    UTerm::Redex { function: Box::new(f), args: all_args }
                }
            },
        )
    ))(input)
}

#[allow(clippy::redundant_closure)]
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

// TODO: use the provided one from nom after it supports empty separator. See
// https://github.com/rust-bakery/nom/pull/1491
pub fn separated_list0<I, O, O2, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: Clone + InputLength,
        F: Parser<I, O, E>,
        G: Parser<I, O2, E>,
        E: ParseError<I>,
{
    move |mut i: I| {
        let mut res = Vec::new();

        match f.parse(i.clone()) {
            Err(nom::Err::Error(_)) => return Ok((i, res)),
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                res.push(o);
                i = i1;
            }
        }

        loop {
            match sep.parse(i.clone()) {
                Err(nom::Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => {
                    match f.parse(i1.clone()) {
                        Err(nom::Err::Error(_)) => return Ok((i, res)),
                        Err(e) => return Err(e),
                        Ok((i2, o)) => {
                            res.push(o);
                            i = i2;
                        }
                    }
                }
            }
        }
    }
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

pub fn infix<I, O1, O2, E, F, G>(mut operator: F, mut operand: G) -> impl FnMut(I) -> IResult<I, (O2, Option<(O1, O2)>), E>
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
            Err(_) => return Ok((i, (first, None))),
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
        Ok((i, (first, Some((middle, last)))))
    }
}


type BoxedUTermParser = Box<dyn FnMut(Input) -> IResult<Input, UTerm>>;

fn operator_call(operators: &'static [OperatorAndName], fixity: Fixity, mut component: BoxedUTermParser) -> BoxedUTermParser {
    Box::new(
        move |input|
            match fixity {
                Infixl => context("infixl_operator", map(
                    infixl(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(head, rest)| {
                        rest.into_iter().fold(head, |acc, (op, arg)| UTerm::Redex { function: Box::new(op), args: vec![acc, arg] })
                    },
                ))(input),
                Infixr => context("infixr_operator", map(
                    infixr(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(init, last)| {
                        init.into_iter().rfold(last, |acc, (arg, op)| UTerm::Redex { function: Box::new(op), args: vec![acc, arg] })
                    },
                ))(input),
                Infix => context("infix_operator", map(
                    infix(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(first, middle_last)| match middle_last {
                        None => first,
                        Some((middle, last)) => UTerm::Redex { function: Box::new(middle), args: vec![first, last] }
                    },
                ))(input),
                Prefix => context("prefix_operator", map(
                    pair(opt(operator_id(operators)), |input| (*component)(input)),
                    |(operator, operand)| match operator {
                        None => operand,
                        Some(operator) => UTerm::Redex { function: Box::new(operator), args: vec![operand] },
                    },
                ))(input),
                Postfix => context("postfix_operator", map(
                    pair(|input| (*component)(input), opt(operator_id(operators))),
                    |(operand, operator)| match operator {
                        None => operand,
                        Some(operator) => UTerm::Redex { function: Box::new(operator), args: vec![operand] },
                    },
                ))(input),
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
    context("expr", scoped(expr_impl))(input)
}

fn lambda(input: Input) -> IResult<Input, UTerm> {
    context("lambda", scoped(
        map(
            pair(
                delimited(
                    token("\\"),
                    separated_list0(newline_opt, id),
                    token("=>")),
                expr),
            |(arg_names, body)| UTerm::Lambda { arg_names, body: Box::new(body) },
        )))(input)
}

fn case(input: Input) -> IResult<Input, UTerm> {
    context("case", scoped(
        map(
            tuple((
                preceded(token("case_int"), map(expr, Box::new)),
                many0(map(preceded(newline, scoped(tuple((int, preceded(token("=>"), u_term))))), |(i, branch)| (i, branch))),
                preceded(newline, scoped(preceded(pair(token("_"), token("=>")), opt(map(u_term, Box::new))))),
            )),
            |(t, branch_entries, default_branch)| {
                let branches = HashMap::from_iter(branch_entries);
                UTerm::CaseInt { t, branches, default_branch }
            })
    ))(input)
}

fn let_term(input: Input) -> IResult<Input, UTerm> {
    context("let_term", map(
        tuple((
            alt((
                scoped(pair(delimited(token("let"), id, token("=")), preceded(newline_opt, u_term))),
                map(expr, |t| (String::from("_"), t)),
            )),
            preceded(newline, u_term),
        )),
        |((name, t), body)| {
            UTerm::Let { name, t: Box::new(t), body: Box::new(body) }
        }))(input)
}

fn defs_term(input: Input) -> IResult<Input, UTerm> {
    context("defs_term", map(
        pair(
            many1(
                map(
                    scoped(
                        tuple((
                            preceded(token("def"), id),
                            many0(id),
                            preceded(preceded(token("=>"), newline_opt), map(u_term, Box::new))))),
                    |(name, args, body)| (name, Def { args, body }),
                )
            ), opt(preceded(newline, map(u_term, Box::new)))),
        |(defs, body)| UTerm::Defs { defs: HashMap::from_iter(defs), body },
    ))(input)
}

fn computaiton(input: Input) -> IResult<Input, UTerm> {
    alt((let_term, defs_term, expr, lambda, case))(input)
}

fn thunk(input: Input) -> IResult<Input, UTerm> {
    context(
        "thunk",
        scoped(
            map(
                preceded(preceded(token("thunk"), newline_opt), computaiton),
                |t| UTerm::Thunk { computation: Box::new(t) })
        ))(input)
}

fn u_term(input: Input) -> IResult<Input, UTerm> {
    context("u_term", alt((thunk, computaiton)))(input)
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let input = Span::new(input);
    let (input, tokens) = tokens(input).map_err(|e| format!("lex error: {:?}", e))?;
    if !input.is_empty() {
        return Err(format!("lex error: unexpected character at ({:?}:{:?}): {:?}", input.location_line(), input.naive_get_utf8_column(), input.lines().next().unwrap()));
    }
    Ok(tokens)
}

pub fn parse_u_term(input: &str) -> Result<UTerm, String> {
    let tokens = tokenize(input)?;
    let input = Input { tokens: &tokens, current_indent: 0 };
    let (input, term) = u_term(input).map_err(|e| format!("parse error: {:?}", e))?;
    if input.tokens.iter().any(|token| !matches!(token, Token::Indent(_))) {
        return Err(format!("parse error: unexpected token at {:?}", input.tokens.first()));
    }
    Ok(term)
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse_u_term, Token, tokenize};
    use crate::test_utils::debug_print;

    #[test]
    fn check_tokenize() -> Result<(), String> {
        let result = tokenize(r#"a b "c" ++- (+) () ,.~
  x
    y

  z"#)?;
        assert_eq!(result, vec![
            Token::Normal("a".to_string(), 0),
            Token::Normal("b".to_string(), 2),
            Token::Str("c".to_string(), 4),
            Token::Normal("++-".to_string(), 8),
            Token::Normal("(".to_string(), 12),
            Token::Normal("+".to_string(), 13),
            Token::Normal(")".to_string(), 14),
            Token::Normal("(".to_string(), 16),
            Token::Normal(")".to_string(), 17),
            Token::Normal(",".to_string(), 19),
            Token::Normal(".~".to_string(), 20),
            Token::Indent(2),
            Token::Normal("x".to_string(), 2),
            Token::Indent(4),
            Token::Normal("y".to_string(), 4),
            Token::Indent(2),
            Token::Normal("z".to_string(), 2),
        ]);
        Ok(())
    }

    #[test]
    fn check_literals() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("123")?), "Int {
    value: 123,
}");
        assert_eq!(debug_print(parse_u_term(r#""abc""#)?), r#"Str {
    value: "abc",
}"#);
        Ok(())
    }

    #[test]
    fn check_parse_simple_expression() -> Result<(), String> {
        let result = parse_u_term("a + b - c * d / -e % +f")?;
        assert_eq!(debug_print(result), r#"Redex {
    function: Identifier {
        name: "_int_sub",
    },
    args: [
        Redex {
            function: Identifier {
                name: "_int_add",
            },
            args: [
                Identifier {
                    name: "a",
                },
                Identifier {
                    name: "b",
                },
            ],
        },
        Redex {
            function: Identifier {
                name: "_int_mod",
            },
            args: [
                Redex {
                    function: Identifier {
                        name: "_int_div",
                    },
                    args: [
                        Redex {
                            function: Identifier {
                                name: "_int_mul",
                            },
                            args: [
                                Identifier {
                                    name: "c",
                                },
                                Identifier {
                                    name: "d",
                                },
                            ],
                        },
                        Redex {
                            function: Identifier {
                                name: "_int_neg",
                            },
                            args: [
                                Identifier {
                                    name: "e",
                                },
                            ],
                        },
                    ],
                },
                Redex {
                    function: Identifier {
                        name: "_int_pos",
                    },
                    args: [
                        Identifier {
                            name: "f",
                        },
                    ],
                },
            ],
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_empty_array() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("{}")?), r#"Struct {
    values: [],
}"#);
        Ok(())
    }


    #[test]
    fn check_single_element_array() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("{a}")?), r#"Struct {
    values: [
        Identifier {
            name: "a",
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_array() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("{a, b, c}")?), r#"Struct {
    values: [
        Identifier {
            name: "a",
        },
        Identifier {
            name: "b",
        },
        Identifier {
            name: "c",
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_mem_get() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("{a, b, c} @ 1")?), r#"MemGet {
    base: Struct {
        values: [
            Identifier {
                name: "a",
            },
            Identifier {
                name: "b",
            },
            Identifier {
                name: "c",
            },
        ],
    },
    offset: Int {
        value: 1,
    },
}"#);
        Ok(())
    }

    #[test]
    fn check_expr_with_parentheses() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("(a + b) * c")?), r#"Redex {
    function: Identifier {
        name: "_int_mul",
    },
    args: [
        Redex {
            function: Identifier {
                name: "_int_add",
            },
            args: [
                Identifier {
                    name: "a",
                },
                Identifier {
                    name: "b",
                },
            ],
        },
        Identifier {
            name: "c",
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_expr_with_parentheses2() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("f (g a)")?), r#"Redex {
    function: Identifier {
        name: "f",
    },
    args: [
        Redex {
            function: Identifier {
                name: "g",
            },
            args: [
                Identifier {
                    name: "a",
                },
            ],
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_scoped_app() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("\
f a b
  g 1 2
  h 3")?), r#"Redex {
    function: Identifier {
        name: "f",
    },
    args: [
        Identifier {
            name: "a",
        },
        Identifier {
            name: "b",
        },
        Let {
            name: "_",
            t: Redex {
                function: Identifier {
                    name: "g",
                },
                args: [
                    Int {
                        value: 1,
                    },
                    Int {
                        value: 2,
                    },
                ],
            },
            body: Redex {
                function: Identifier {
                    name: "h",
                },
                args: [
                    Int {
                        value: 3,
                    },
                ],
            },
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_force() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("force x y z")?), r#"Redex {
    function: Force {
        thunk: Identifier {
            name: "x",
        },
    },
    args: [
        Identifier {
            name: "y",
        },
        Identifier {
            name: "z",
        },
    ],
}"#);
        Ok(())
    }

    #[test]
    fn check_thunk() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("thunk \\=> a b c")?), r#"Thunk {
    computation: Lambda {
        arg_names: [],
        body: Redex {
            function: Identifier {
                name: "a",
            },
            args: [
                Identifier {
                    name: "b",
                },
                Identifier {
                    name: "c",
                },
            ],
        },
    },
}"#);
        Ok(())
    }

    #[test]
    fn check_let() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("
let x = 1
x")?), r#"Let {
    name: "x",
    t: Int {
        value: 1,
    },
    body: Identifier {
        name: "x",
    },
}"#);
        Ok(())
    }

    #[test]
    fn check_case_int() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("case_int 1
  1 => 2
  _ => 3
")?), r#"CaseInt {
    t: Int {
        value: 1,
    },
    branches: {
        1: Int {
            value: 2,
        },
    },
    default_branch: Some(
        Int {
            value: 3,
        },
    ),
}"#);
        Ok(())
    }

    #[test]
    fn check_mem_access() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term("let a = {1, 2, 3}
a @ 0 = 4
a @ 0 + a @ 1")?), r#"Let {
    name: "a",
    t: Struct {
        values: [
            Int {
                value: 1,
            },
            Int {
                value: 2,
            },
            Int {
                value: 3,
            },
        ],
    },
    body: Let {
        name: "_",
        t: MemSet {
            base: Identifier {
                name: "a",
            },
            offset: Int {
                value: 0,
            },
            value: Int {
                value: 4,
            },
        },
        body: Redex {
            function: Identifier {
                name: "_int_add",
            },
            args: [
                MemGet {
                    base: Identifier {
                        name: "a",
                    },
                    offset: Int {
                        value: 0,
                    },
                },
                MemGet {
                    base: Identifier {
                        name: "a",
                    },
                    offset: Int {
                        value: 1,
                    },
                },
            ],
        },
    },
}"#);
        Ok(())
    }

    #[test]
    fn check_def() -> Result<(), String> {
        assert_eq!(debug_print(parse_u_term(r#"
def f x => x
def g x y => x + y
g (f 1) 2
"#)?), r#"Defs {
    defs: {
        "f": Def {
            args: [
                "x",
            ],
            body: Identifier {
                name: "x",
            },
        },
    },
    body: Some(
        Defs {
            defs: {
                "g": Def {
                    args: [
                        "x",
                        "y",
                    ],
                    body: Redex {
                        function: Identifier {
                            name: "_int_add",
                        },
                        args: [
                            Identifier {
                                name: "x",
                            },
                            Identifier {
                                name: "y",
                            },
                        ],
                    },
                },
            },
            body: Some(
                Redex {
                    function: Identifier {
                        name: "g",
                    },
                    args: [
                        Redex {
                            function: Identifier {
                                name: "f",
                            },
                            args: [
                                Int {
                                    value: 1,
                                },
                            ],
                        },
                        Int {
                            value: 2,
                        },
                    ],
                },
            ),
        },
    ),
}"#);
        Ok(())
    }
}