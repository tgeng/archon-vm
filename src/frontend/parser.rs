use nom::branch::alt;
use nom::{InputLength, IResult};
use nom::character::complete::{space0, char, satisfy as char_satisfy, alphanumeric1, one_of};
use nom::combinator::{cut, map, map_res, opt};
use nom::bytes::complete::{take_while1, escaped};
use nom::error::{context, ErrorKind, ParseError};
use nom::Parser;
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use crate::frontend::f_term::{Def, FTerm};
use nom_locate::{LocatedSpan};
use crate::frontend::parser::Fixity::{*};
use crate::ast::term::{CType, SpecializedType, PType, VType};

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
    "let", "def", "case", "force", "thunk", "=>", "=", "(", ")", ",", "\\", "{", "}", "@", "_", ":", "->"
];

// tokenizer

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Normal(String, u32, usize),
    Int(i64, u32, usize),
    Str(String, u32, usize),
    Indent(u32, usize),
}

impl Token {
    fn column(&self) -> usize {
        match self {
            Token::Normal(_, _, column) => *column,
            Token::Int(_, _, column) => *column,
            Token::Str(_, _, column) => *column,
            Token::Indent(_, column) => *column,
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
                        Token::Normal(id_string, input.location_line() - 1, input.naive_get_utf8_column() - 1)
                    }),
                // specially handle some punctuations that should never be combined with others
                one_of("(),\\{}").map(|c| Token::Normal(c.to_string(), input.location_line() - 1, input.naive_get_utf8_column() - 1)),
                // punctuation identifier
                take_while1(|c: char| c.is_ascii_punctuation() &&
                    c != '`' && c != '"' && c != '(' && c != ')' && c != ',' && c != '\\')
                    .map(|s: Span| Token::Normal(s.to_string(), input.location_line() - 1, input.naive_get_utf8_column() - 1)),
                // backtick-quoted identifier
                delimited(
                    char('`'),
                    take_while1(|c| c != '`'), char('`')).map(|s: Span| Token::Normal(s.to_string(), input.location_line() - 1, input.naive_get_utf8_column() - 1)),
            ),
        ))(input)
}

fn int_token(input: Span) -> IResult<Span, Token> {
    context(
        "int_token",
        map(
            take_while1(|c: char| c.is_ascii_digit()),
            |s: Span| Token::Int(s.to_string().parse::<i64>().unwrap(), input.location_line() - 1, s.naive_get_utf8_column() - 1),
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
            |s: Span| Token::Str(s.to_string(), input.location_line() - 1, s.naive_get_utf8_column() - 2), // offset quote
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
                    Ok(Token::Indent(input.location_line(), indent))
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

fn v_type(input: Input) -> IResult<Input, VType> {
    map(
        alt((
            token("uni").map(|_| VType::Uniform),
            token("int").map(|_| VType::Specialized(SpecializedType::Integer)),
            token("sptr").map(|_| VType::Specialized(SpecializedType::StructPtr)),
            token("pptr").map(|_| VType::Specialized(SpecializedType::PrimitivePtr)),
            token("i64").map(|_| VType::Specialized(SpecializedType::Primitive(PType::I64))),
            token("f64").map(|_| VType::Specialized(SpecializedType::Primitive(PType::F64))),
            token("i32").map(|_| VType::Specialized(SpecializedType::Primitive(PType::I32))),
            token("f32").map(|_| VType::Specialized(SpecializedType::Primitive(PType::F32))),
        )),
        |v_type| v_type,
    )(input)
}

fn v_type_decl(input: Input) -> IResult<Input, VType> {
    map(
        opt(preceded(token(":"), v_type)),
        |v_type_option| v_type_option.unwrap_or(VType::Uniform),
    )(input)
}

fn specialized_c_type(input: Input) -> IResult<Input, CType> {
    map(v_type, CType::SpecializedF)(input)
}

fn c_type_decl(input: Input) -> IResult<Input, CType> {
    map(
        opt(preceded(token("->"), specialized_c_type)),
        |c_type_option| c_type_option.unwrap_or(CType::Default),
    )(input)
}

fn newline(input: Input) -> IResult<Input, ()> {
    let current_indent = input.current_indent;
    // This local variable is needed to make rust's borrow checker happy. Otherwise it complains
    // `current_indent` does not live long enough.
    let mut p = satisfy(|token| matches!(token,Token::Indent(_, column) if *column >= current_indent ));
    p(input)
}

fn newline_opt(input: Input) -> IResult<Input, ()> {
    opt(newline)(input).map(|(input, _)| (input, ()))
}

fn token(s: &'static str) -> impl FnMut(Input) -> IResult<Input, ()> {
    move |input| {
        context(
            s,
            satisfy(move |token| matches!(token, Token::Normal(name, _, _) if { name == s })),
        )(input)
    }
}

fn id(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Normal(name, _, _)
        if KEYWORDS.iter().all(|k| k != name) &&
            PRECEDENCE.iter().all(|(names, ..)| names.iter().all(|(n, _)| n != name))
        => Some(name.clone()),
        _ => None,
    })(input)
}

fn id_term(input: Input) -> IResult<Input, FTerm> {
    context("id_term", map(id, |name| FTerm::Identifier { name }))(input)
}

fn int(input: Input) -> IResult<Input, i64> {
    map_token(|token| match token {
        Token::Int(value, _, _) => Some(*value),
        _ => None,
    })(input)
}

fn int_term(input: Input) -> IResult<Input, FTerm> {
    context("int_term", map(int, |value| FTerm::Int { value }))(input)
}

fn str(input: Input) -> IResult<Input, String> {
    map_token(|token| match token {
        Token::Str(value, _, _) => Some(value.clone()),
        _ => None,
    })(input)
}

fn str_term(input: Input) -> IResult<Input, FTerm> {
    context("str_term", map(str, |value| FTerm::Str { value }))(input)
}

fn struct_(input: Input) -> IResult<Input, FTerm> {
    context("struct", map(
        delimited(
            token("{"),
            cut(separated_list0(
                delimited(newline_opt, token(","), newline_opt),
                expr)),
            token("}"),
        ),
        |values: Vec<FTerm>| FTerm::Struct { values },
    ))(input)
}

fn atom(input: Input) -> IResult<Input, FTerm> {
    context("atom", alt((
        id_term,
        int_term,
        str_term,
        struct_,
        delimited(
            pair(token("("), newline_opt),
            cut(f_term),
            pair(newline_opt, token(")"))),
    )))(input)
}

fn force(input: Input) -> IResult<Input, FTerm> {
    context("force", map(preceded(token("force"), cut(atom)), |t| FTerm::Force { thunk: Box::new(t) }))(input)
}

fn mem_access_or_atom(input: Input) -> IResult<Input, FTerm> {
    context("mem_access_or_atom", map(
        pair(atom, many0(pair(preceded(token("@"), cut(atom)), opt(preceded(token("="), cut(f_term)))))),
        |(t, index_and_values)| {
            index_and_values.into_iter().fold(t, |t, (index, assignment)| {
                match assignment {
                    None => FTerm::MemGet { base: Box::new(t), offset: Box::new(index) },
                    Some(value) => FTerm::MemSet { base: Box::new(t), offset: Box::new(index), value: Box::new(value) },
                }
            })
        },
    ))(input)
}

fn scoped_app(input: Input) -> IResult<Input, FTerm> {
    context("scoped_app", scoped(
        map(
            pair(many1(alt((mem_access_or_atom, force))), many0(preceded(newline, f_term))),
            |(f_and_args, more_args)| {
                if f_and_args.len() == 1 && more_args.is_empty() {
                    f_and_args.into_iter().next().unwrap()
                } else {
                    let f = f_and_args.first().unwrap().clone();
                    let all_args = f_and_args.into_iter().skip(1).chain(more_args).collect();
                    FTerm::Redex { function: Box::new(f), args: all_args }
                }
            },
        )
    ))(input)
}

#[allow(clippy::redundant_closure)]
fn scoped_app_boxed() -> BoxedFTermParser {
    Box::new(move |input| scoped_app(input))
}

fn operator_id(operators: &'static [OperatorAndName]) -> impl FnMut(Input) -> IResult<Input, FTerm> {
    map_token(|token| match token {
        Token::Normal(name, _, _) => {
            operators.iter()
                .filter_map(|(op, fun_name)|
                    if op == name {
                        Some(fun_name)
                    } else {
                        None
                    })
                .next()
                .map(|fun_name| FTerm::Identifier { name: fun_name.to_string() })
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


type BoxedFTermParser = Box<dyn FnMut(Input) -> IResult<Input, FTerm>>;

fn operator_call(operators: &'static [OperatorAndName], fixity: Fixity, mut component: BoxedFTermParser) -> BoxedFTermParser {
    Box::new(
        move |input|
            match fixity {
                Infixl => context("infixl_operator", map(
                    infixl(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(head, rest)| {
                        rest.into_iter().fold(head, |acc, (op, arg)| FTerm::Redex { function: Box::new(op), args: vec![acc, arg] })
                    },
                ))(input),
                Infixr => context("infixr_operator", map(
                    infixr(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(init, last)| {
                        init.into_iter().rfold(last, |acc, (arg, op)| FTerm::Redex { function: Box::new(op), args: vec![acc, arg] })
                    },
                ))(input),
                Infix => context("infix_operator", map(
                    infix(delimited(newline_opt, operator_id(operators), newline_opt), |input| (*component)(input)),
                    |(first, middle_last)| match middle_last {
                        None => first,
                        Some((middle, last)) => FTerm::Redex { function: Box::new(middle), args: vec![first, last] }
                    },
                ))(input),
                Prefix => context("prefix_operator", map(
                    pair(opt(operator_id(operators)), |input| (*component)(input)),
                    |(operator, operand)| match operator {
                        None => operand,
                        Some(operator) => FTerm::Redex { function: Box::new(operator), args: vec![operand] },
                    },
                ))(input),
                Postfix => context("postfix_operator", map(
                    pair(|input| (*component)(input), opt(operator_id(operators))),
                    |(operand, operator)| match operator {
                        None => operand,
                        Some(operator) => FTerm::Redex { function: Box::new(operator), args: vec![operand] },
                    },
                ))(input),
            }
    )
}

fn expr_impl(input: Input) -> IResult<Input, FTerm> {
    let mut p = PRECEDENCE.iter().fold(scoped_app_boxed(), |f, (operators, fixity)| {
        operator_call(operators, *fixity, f)
    });
    (*p)(input)
}

fn expr(input: Input) -> IResult<Input, FTerm> {
    context("expr", scoped(expr_impl))(input)
}

fn lambda(input: Input) -> IResult<Input, FTerm> {
    context("lambda", scoped(
        map(
            pair(
                delimited(
                    token("\\"),
                    separated_list0(newline_opt, pair(id, v_type_decl)),
                    token("=>")),
                cut(expr)),
            |(arg_names, body)| FTerm::Lambda { arg_names, body: Box::new(body) },
        )))(input)
}

fn case(input: Input) -> IResult<Input, FTerm> {
    context("case", scoped(
        map(
            tuple((
                preceded(token("case"), map(expr, Box::new)),
                c_type_decl,
                many0(map(preceded(newline, scoped(tuple((int, preceded(token("=>"), f_term))))), |(i, branch)| (i, branch))),
                opt(preceded(newline, scoped(preceded(pair(token("_"), token("=>")), map(f_term, Box::new))))),
            )),
            |(t, result_type, branches, default_branch)| {
                FTerm::CaseInt { t, result_type, branches, default_branch }
            })
    ))(input)
}

fn let_term(input: Input) -> IResult<Input, FTerm> {
    context("let_term", map(
        tuple((
            alt((
                scoped(pair(delimited(token("let"), id, token("=")), preceded(newline_opt, cut(f_term)))),
                map(expr, |t| (String::from("_"), t)),
            )),
            preceded(newline, f_term),
        )),
        |((name, t), body)| {
            FTerm::Let { name, t: Box::new(t), body: Box::new(body) }
        }))(input)
}

fn defs_term(input: Input) -> IResult<Input, FTerm> {
    context("defs_term", map(
        pair(
            many1(
                map(
                    scoped(
                        tuple((
                            preceded(token("def"), cut(id)),
                            many0(pair(id, v_type_decl)),
                            c_type_decl,
                            preceded(preceded(token("=>"), newline_opt), map(cut(f_term), Box::new))))),
                    |(name, args, c_type, body)| (name, Def { args, c_type, body }),
                )
            ), opt(preceded(newline, map(f_term, Box::new)))),
        |(defs, body)| FTerm::Defs { defs, body },
    ))(input)
}

fn computation(input: Input) -> IResult<Input, FTerm> {
    alt((let_term, defs_term, expr, lambda, case))(input)
}

fn thunk(input: Input) -> IResult<Input, FTerm> {
    context(
        "thunk",
        scoped(
            map(
                preceded(preceded(token("thunk"), newline_opt), cut(computation)),
                |t| FTerm::Thunk { computation: Box::new(t) })
        ))(input)
}

fn f_term(input: Input) -> IResult<Input, FTerm> {
    context("f_term", alt((thunk, computation)))(input)
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let input = Span::new(input);
    let (input, tokens) = tokens(input).map_err(|e| format!("lex error: {:?}", e))?;
    if !input.is_empty() {
        return Err(format!("lex error: unexpected character at ({:?}:{:?}): {:?}", input.location_line(), input.naive_get_utf8_column(), input.lines().next().unwrap()));
    }
    Ok(tokens)
}

pub fn parse_f_term(input: &str) -> Result<FTerm, String> {
    let tokens = tokenize(input)?;
    let input = Input { tokens: &tokens, current_indent: 0 };
    let (input, term) = f_term(input).map_err(|e| format!("parse error: {:?}", e))?;
    if input.tokens.iter().any(|token| !matches!(token, Token::Indent(_, _))) {
        return Err(format!("parse error: unexpected token at {:?}", input.tokens.first()));
    }
    Ok(term)
}

#[cfg(test)]
mod tests {
    use crate::frontend::parser::{parse_f_term, tokenize};
    use crate::test_utils::debug_print;

    #[test]
    fn check_tokenize() -> Result<(), String> {
        let result = tokenize(r#"a b "c" ++- (+) () ,.~
  x
    y

  z"#)?;
        assert_eq!(format!("{:#?}", result), r#"[
    Normal(
        "a",
        0,
        0,
    ),
    Normal(
        "b",
        0,
        2,
    ),
    Str(
        "c",
        0,
        4,
    ),
    Normal(
        "++-",
        0,
        8,
    ),
    Normal(
        "(",
        0,
        12,
    ),
    Normal(
        "+",
        0,
        13,
    ),
    Normal(
        ")",
        0,
        14,
    ),
    Normal(
        "(",
        0,
        16,
    ),
    Normal(
        ")",
        0,
        17,
    ),
    Normal(
        ",",
        0,
        19,
    ),
    Normal(
        ".~",
        0,
        20,
    ),
    Indent(
        1,
        2,
    ),
    Normal(
        "x",
        1,
        2,
    ),
    Indent(
        2,
        4,
    ),
    Normal(
        "y",
        2,
        4,
    ),
    Indent(
        3,
        2,
    ),
    Normal(
        "z",
        4,
        2,
    ),
]"#);
        Ok(())
    }

    #[test]
    fn check_literals() -> Result<(), String> {
        assert_eq!(debug_print(parse_f_term("123")?), "Int {
    value: 123,
}");
        assert_eq!(debug_print(parse_f_term(r#""abc""#)?), r#"Str {
    value: "abc",
}"#);
        Ok(())
    }

    #[test]
    fn check_parse_simple_expression() -> Result<(), String> {
        let result = parse_f_term("a + b - c * d / -e % +f")?;
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
        assert_eq!(debug_print(parse_f_term("{}")?), r#"Struct {
    values: [],
}"#);
        Ok(())
    }


    #[test]
    fn check_single_element_array() -> Result<(), String> {
        assert_eq!(debug_print(parse_f_term("{a}")?), r#"Struct {
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
        assert_eq!(debug_print(parse_f_term("{a, b, c}")?), r#"Struct {
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
        assert_eq!(debug_print(parse_f_term("{a, b, c} @ 1")?), r#"MemGet {
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
        assert_eq!(debug_print(parse_f_term("(a + b) * c")?), r#"Redex {
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
        assert_eq!(debug_print(parse_f_term("f (g a)")?), r#"Redex {
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
        assert_eq!(debug_print(parse_f_term("\
f a b (c d)
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
        Redex {
            function: Identifier {
                name: "c",
            },
            args: [
                Identifier {
                    name: "d",
                },
            ],
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
        assert_eq!(debug_print(parse_f_term("force x y z")?), r#"Redex {
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
        assert_eq!(debug_print(parse_f_term("thunk \\=> a b c")?), r#"Thunk {
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
        assert_eq!(debug_print(parse_f_term("
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
        assert_eq!(debug_print(parse_f_term("case 1 -> int
  1 => 2
  _ => 3
")?), r#"CaseInt {
    t: Int {
        value: 1,
    },
    result_type: SpecializedF(
        Specialized(
            Integer,
        ),
    ),
    branches: [
        (
            1,
            Int {
                value: 2,
            },
        ),
    ],
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
        assert_eq!(debug_print(parse_f_term("let a = {1, 2, 3}
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
    fn check_nested_mem_access() -> Result<(), String> {
        assert_eq!(debug_print(parse_f_term("{1, {2}, 3} @ 1 @ 0 = 4")?), r#"MemSet {
    base: MemGet {
        base: Struct {
            values: [
                Int {
                    value: 1,
                },
                Struct {
                    values: [
                        Int {
                            value: 2,
                        },
                    ],
                },
                Int {
                    value: 3,
                },
            ],
        },
        offset: Int {
            value: 1,
        },
    },
    offset: Int {
        value: 0,
    },
    value: Int {
        value: 4,
    },
}"#);
        Ok(())
    }

    #[test]
    fn check_def() -> Result<(), String> {
        assert_eq!(debug_print(parse_f_term(r#"
def f x => x
def g x y => x + y
g (f 1) 2
"#)?), r#"Defs {
    defs: [
        (
            "f",
            Def {
                args: [
                    (
                        "x",
                        Uniform,
                    ),
                ],
                body: Identifier {
                    name: "x",
                },
                c_type: Default,
            },
        ),
    ],
    body: Some(
        Defs {
            defs: [
                (
                    "g",
                    Def {
                        args: [
                            (
                                "x",
                                Uniform,
                            ),
                            (
                                "y",
                                Uniform,
                            ),
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
                        c_type: Default,
                    },
                ),
            ],
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