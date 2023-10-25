use nom::branch::alt;
use nom::IResult;
use nom::character::complete::{line_ending, space0, space1, char, satisfy, alphanumeric1, one_of};
use nom::combinator::{cut, map, verify};
use nom::bytes::complete::{take_while1, escaped, tag};
use nom::error::{context};
use nom::Parser;
use nom::multi::{many1, many_m_n, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated};
use crate::u_term::UTerm;
use nom_locate::{LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

// keywords
static KEYWORDS: &[&str] = &[
    "let", "def", "case_int", "case_str", "case_tuple", "force", "thunk",
];

fn identifier(input: Span) -> IResult<Span, UTerm> {
    map(
        verify(
            alt((
                    // alpha-numeric identifier
                    satisfy(|c| c.is_alphabetic() || c == '_')
                        .and(take_while1(|c: char| c.is_alphanumeric() || c == '_'))
                        .map(|(head, tail)| format!("{}{}", head, tail)),
                    // punctuation identifier
                    take_while1(|c: char| c.is_ascii_punctuation() && c != '`' && c != '(' && c != ')' && c != ',').map(|s: Span| s.to_string()),
                    // backtick-quoted identifier
                    delimited(
                        char('`'),
                        take_while1(|c| c != '`'), char('`')).map(|s: Span| s.to_string()),
                ),
            ),
            |result: &str| KEYWORDS.iter().all(|&keyword| result != keyword),
        ),
        |s| UTerm::Identifier { name: s },
    )(input)
}

fn int(input: Span) -> IResult<Span, UTerm> {
    map(
        take_while1(|c: char| c.is_ascii_digit()),
        |s: Span| UTerm::Int { value: s.to_string().parse::<i32>().unwrap() },
    )(input)
}

fn str(input: Span) -> IResult<Span, UTerm> {
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
        |s: Span| UTerm::Str { value: s.to_string() },
    )(input)
}

fn empty_tuple(input: Span) -> IResult<Span, UTerm> {
    map(tag("(("), |_| UTerm::Tuple { values: vec![] })(input)
}

struct ParsingContext {
    indent: usize,
}

impl ParsingContext {
    fn indented<'a, F, R>(&'a self, f: F) -> impl FnOnce(Span<'a>) -> R where F: FnOnce(ParsingContext) -> R {
        move |span: Span<'a>| {
            f(ParsingContext { indent: span.naive_get_utf8_column() })
        }
    }

    fn sp<'a>(&self) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, ()> {
        alt((
            many1(space0.and(line_ending))
                .and(many_m_n(self.indent, self.indent, char(' ')))
                .and(many1(char(' '))) // consume additional spaces
                .map(|_| ()),
            space1.map(|_| ()),
        ))
    }

    // Instead of returning an opaque type, we just declare a function to workaround rust's issue
    // with instantiating infinite opaque types due to recursive calls.
    fn tuple_or_term_<'a>(&'a self, s: Span<'a>, inside_parentheses: bool) -> IResult<Span<'a>, UTerm> {
        map(
            separated_list0(delimited(self.sp(), tag(","), self.sp()), self.expr()),
            |values| {
                if values.len() == 1 && !inside_parentheses {
                    values.into_iter().next().unwrap()
                } else {
                    UTerm::Tuple { values }
                }
            },
        )(s)
    }
    fn tuple_or_term<'a>(&'a self, inside_parentheses: bool) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
        move |s: Span<'a>| self.tuple_or_term_(s, inside_parentheses)
    }

    fn expr<'a>(&'a self) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
        self.atom()
    }

    fn atom<'a>(&'a self) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, UTerm> {
        alt((
            identifier,
            int,
            str,
            delimited(
                pair(char('('), self.sp()),
                self.tuple_or_term(true),
                pair(self.sp(), char(')')))))
    }
}


pub fn parse_u_term(input: &str) -> Result<UTerm, String> {
    let ctx = ParsingContext { indent: 0 };
    let r = ctx.tuple_or_term_(Span::new(input), false);
    match r {
        Ok((_, result)) => Ok(result),
        err => Err(format!("Parse error: {:?}", err)),
    }
}
