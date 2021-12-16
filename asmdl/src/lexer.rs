use crate::ast;
use nom::{
    branch, character, combinator,
    error::{make_error, ErrorKind, ParseError},
    multi, IResult,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Unknown,
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Directive(String),
    GlobalIdentifier(ast::Identifier),
    LocalIdentifier(ast::Identifier),
    LiteralInteger(i128),
    /// A string enclosed in quotation marks.
    LiteralString(String),
    /// An instruction or other keyword.
    Keyword(String),
}

#[derive(Debug)]
pub struct PositionedToken {
    pub token: Token,
    pub position: usize,
}

fn ignore_parser<'a, O, E: ParseError<&'a str>, F>(
    mut parser: F,
) -> impl FnMut(&'a str) -> IResult<&str, (), E>
where
    F: nom::Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, _) = parser.parse(input)?;
        Ok((input, ()))
    }
}

fn keyword_char(input: &str, allow_digit: bool) -> IResult<&str, char> {
    let (input, c) = character::complete::anychar(input)?;
    if c.is_alphanumeric() || c == '.' || (allow_digit && c.is_digit(10u32)) {
        Ok((input, c))
    } else {
        Err(nom::Err::Error(make_error(input, ErrorKind::Alpha))) // TODO: More specific error kind for keyword character.
    }
}

fn keyword(input: &str) -> IResult<&str, String> {
    let (input, first_char) = keyword_char(input, false)?;
    let (input, mut remaining_chars) = multi::many0(|input| keyword_char(input, true))(input)?;
    remaining_chars.insert(0, first_char);
    Ok((input, remaining_chars.into_iter().collect()))
}

fn whitespace_or_comments(input: &str) -> IResult<&str, ()> {
    ignore_parser(multi::many0(ignore_parser(
        character::complete::multispace1,
    )))(input)
}

fn literal_integer_dec(input: &str) -> IResult<&str, i128> {
    let (input, negative) = combinator::opt(character::complete::char('-'))(input)?;
    let (input, value) = combinator::map_res(character::complete::digit1, |digits: &str| {
        i128::from_str_radix(digits, 10)
    })(input)?;
    Ok((input, value * (if negative.is_some() { -1 } else { 1 })))
}

//fn literal_integer_hex

fn literal_integer(input: &str) -> IResult<&str, i128> {
    literal_integer_dec(input)
}

fn character_token<'a>(c: char, token: Token) -> impl FnMut(&'a str) -> IResult<&'a str, Token> {
    combinator::value(token, character::complete::char(c))
}

fn token<'a>(input: &'a str) -> IResult<&'a str, Token> {
    branch::alt((
        combinator::map(keyword, Token::Keyword),
        combinator::map(literal_integer, Token::LiteralInteger),
        character_token(';', Token::Semicolon),
        character_token('{', Token::OpenBracket),
        character_token('}', Token::CloseBracket),
        character_token('(', Token::OpenParenthesis),
        character_token(')', Token::CloseParenthesis),
        combinator::value(Token::Unknown, character::complete::anychar),
    ))(input)
}

//fn positioned_token

fn token_sequence<'a>(input: &'a str) -> IResult<&'a str, Vec<Token>> {
    let (input, ()) = whitespace_or_comments(input)?;
    multi::many0(|input: &'a str| {
        let (input, token) = token(input)?;
        let (input, ()) = whitespace_or_comments(input)?;
        Ok((input, token))
    })(input)
}

pub fn lex(input: &str) -> Vec<Token> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use crate::lexer::{token, token_sequence, Token};

    #[test]
    fn keyword_test() {
        let instruction: &str = "obj.arr.new";
        assert_eq!(
            token(instruction),
            Ok(("", Token::Keyword(String::from(instruction))))
        );
    }

    #[test]
    fn basic_sequence_test() {
        assert_eq!(
            token_sequence("{ret;42"),
            Ok((
                "",
                vec![
                    Token::OpenBracket,
                    Token::Keyword(String::from("ret")),
                    Token::Semicolon
                ]
            ))
        );
    }
}
