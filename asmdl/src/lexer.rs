use crate::ast;
use nom::{
    character::complete::*,
    error::{ErrorKind, ParseError},
    multi, IResult,
};

#[derive(Debug)]
pub enum Token {
    Unknown,
    OpenBracket,
    CloseBracket,
    Semicolon,
    Directive(String),
    GlobalIdentifier(ast::Identifier),
    LocalIdentifier(ast::Identifier),
    LiteralInteger(isize),
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

fn keyword_char(input: &str, allow_digit: bool) -> IResult<&str, char> {
    let (input, c) = anychar(input)?;
    if c.is_alphanumeric() || c == '.' || (allow_digit && c.is_digit(10u32)) {
        Ok((input, c))
    } else {
        Err(nom::Err::Error(ParseError::from_error_kind(
            input,
            nom::error::ErrorKind::Alpha,
        )))
    }
}

fn keyword(input: &str) -> IResult<&str, String> {
    let (input, first_char) = keyword_char(input, false)?;
    let (input, mut remaining_chars) = multi::many0(|input| keyword_char(input, true))(input)?;
    remaining_chars.insert(0, first_char);
    Ok((input, remaining_chars.into_iter().collect()))
}

#[cfg(test)]
mod tests {
    #[test]
    fn keyword_test() {
        unimplemented!();
    }
}
