use nom::branch::alt;
use nom::{InputLength, IResult};
use nom::character::complete::{line_ending, space0, char, satisfy as char_satisfy, alphanumeric1, one_of, anychar};
use nom::combinator::{cut, map, opt, peek, verify};
use nom::bytes::complete::{take_while1, escaped, tag};
use nom::error::{context};
use nom::Parser;
use nom::multi::{many0, many0_count, many1, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use crate::u_term::UTerm;
use nom_locate::{LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

// keywords
static KEYWORDS: &[&str] = &[
    "let", "def", "case_int", "case_str", "case_tuple", "force", "thunk", "="
];

// tokenizer

#[derive(Debug, Clone)]
enum Token {
    Identifier(String, usize),
    Int(i64),
    Str(String),
    Indent(usize),
}

fn identifier_token(input: Span) -> IResult<Span, Token> {
    alt((
            // alpha-numeric identifier
            char_satisfy(|c| c.is_alphabetic() || c == '_')
                .and(take_while1(|c: char| c.is_alphanumeric() || c == '_'))
                .map(|(head, tail)| Token::Identifier(format!("{}{}", head, tail), input.naive_get_utf8_column())),
            // specially handle some punctuations that should never be combined with others
            one_of("(),\\").map(|c| Token::Identifier(c.to_string(), input.naive_get_utf8_column())),
            // punctuation identifier
            take_while1(|c: char| c.is_ascii_punctuation() && c != '`').map(|s: Span| Token::Identifier(s.to_string(), input.naive_get_utf8_column())),
            // backtick-quoted identifier
            delimited(
                char('`'),
                take_while1(|c| c != '`'), char('`')).map(|s: Span| Token::Identifier(s.to_string(), input.naive_get_utf8_column())),
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
                Token::Identifier(_, column) => f.parse(Input { tokens: input.tokens, current_indent: *column }),
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

fn id_token(s: &'static str) -> impl FnMut(Input) -> IResult<Input, ()> {
    satisfy(move |token| matches!(token, Token::Identifier(name, _) if { name == s }))
}

fn identifier(input: Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Identifier(name, _) if { KEYWORDS.iter().all(|k| k != name) } => Some(UTerm::Identifier { name: name.clone() }),
        _ => None,
    })(input)
}

fn int(input: Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Int(value) => Some(UTerm::Int { value: *value }),
        _ => None,
    })(input)
}

fn str(input: Input) -> IResult<Input, UTerm> {
    map_token(|token| match token {
        Token::Str(value) => Some(UTerm::Str { value: value.clone() }),
        _ => None,
    })(input)
}

fn atom(input: Input) -> IResult<Input, UTerm> {
    alt((
        identifier,
        int,
        str,
        delimited(
            pair(id_token("("), newline_opt),
            u_term,
            pair(newline_opt, id_token(")"))),
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
    scoped_app(input)
}

fn u_term(input: Input) -> IResult<Input, UTerm> {
    scoped(alt((atom, )))(input)
}


// ==========================================
//
// fn empty_tuple(input: Span) -> IResult<Span, UTerm> {
//     map(tag("(("), |_| UTerm::Tuple { values: vec![] })(input)
// }
//
// #[derive(Debug, Clone)]
// struct ParsingContext {}
//
// impl ParsingContext {
//     fn indented<'a, F, R>(&'a self, mut f: F) -> impl FnMut(Span<'a>) -> R where F: FnMut(usize, Span<'a>) -> R {
//         move |span: Span<'a>| {
//             f(span.naive_get_utf8_column(), span)
//         }
//     }
//
//     fn newline<'a>(&self, indent: usize) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, ()> {
//         map(
//             many1(space0.and(line_ending))
//                 .and(many_m_n(indent, indent, char(' ')))
//                 .and(many1(char(' '))), // consume additional spaces
//             |_| (),
//         )
//     }
//
//     fn sp<'a>(&self, indent: usize) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, ()> {
//         alt((self.newline(indent), space1.map(|_| ())))
//     }
//
//     fn u_term<'a>(&'a self, indent: usize, inside_parentheses: bool) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         move |s: Span<'a>| self.u_term_(s, indent, inside_parentheses)
//     }
//
//     // Instead of returning an opaque type, we just declare a function to workaround rust's issue
//     // with instantiating infinite opaque types due to recursive calls.
//     fn u_term_<'a>(&'a self, s: Span<'a>, indent: usize, inside_parentheses: bool) -> IResult<Span<'a>, UTerm> {
//         alt((
//             self.lambda(),
//             self.tuple_or_term(indent, inside_parentheses),
//         ))(s)
//     }
//
//     fn lambda<'a>(&'a self) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         self.atom(0)
//     }
//
//     fn tuple_or_term<'a>(&'a self, indent: usize, inside_parentheses: bool) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         map(
//             separated_list0(delimited(self.sp(indent), tag(","), self.sp(indent)), self.expr(indent)),
//             move |values: Vec<UTerm>| {
//                 if values.len() == 1 && !inside_parentheses {
//                     values.into_iter().next().unwrap()
//                 } else {
//                     UTerm::Tuple { values }
//                 }
//             },
//         )
//     }
//
//     fn expr<'a>(&'a self, indent: usize) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         self.indented_app()
//     }
//
//     fn indented_app<'a>(&'a self) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         self.indented(|indent, span| {
//             map(
//                 tuple((
//                     self.atom(indent),
//                     many0(preceded(self.newline(indent), self.u_term(indent, false))))),
//                 |(f, args): (UTerm, Vec<UTerm>)| {
//                     UTerm::App { function: Box::new(f), args }
//                 })(span)
//         })
//     }
//
//     fn atom<'a>(&'a self, indent: usize) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
//         alt((
//             identifier,
//             int,
//             str,
//             delimited(
//                 pair(char('('), self.sp(indent)),
//                 self.u_term(indent, true),
//                 pair(self.sp(indent), char(')')))))
//     }
// }
//
//
// pub fn parse_u_term(input: &str) -> Result<UTerm, String> {
//     let ctx = ParsingContext {};
//     let r = ctx.indented(|indent, span| {
//         ctx.u_term(indent, false)(span)
//     })(Span::new(input));
//     match r {
//         Ok((_, result)) => Ok(result),
//         err => Err(format!("Parse error: {:?}", err)),
//     }
// }
