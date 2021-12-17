use crate::ast;
use combine::parser::{char, Parser};

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
    pub position: ast::Position,
}

// //fn positioned_token

// fn token_sequence<'a>(input: &'a str) -> IResult<&'a str, Vec<Token>> {
//     let (input, ()) = whitespace_or_comments(input)?;
//     multi::many0(|input: &'a str| {
//         let (input, token) = token(input)?;
//         let (input, ()) = whitespace_or_comments(input)?;
//         Ok((input, token))
//     })(input)
// }

fn period<'a>() -> impl Parser<&'a str> {
    char::char('.')
}

fn newline<'a>() -> impl Parser<&'a str> {
    combine::choice((char::crlf(), char::newline()))
}

fn whitespace_or_comments<'a>() -> impl Parser<&'a str> {
    combine::choice((newline(), newline()))
}

fn directive<'a>() -> impl Parser<&'a str, Output = String> {
    period().with(combine::many1::<String, _, _>(char::alpha_num()))
}

fn keyword<'a>() -> impl Parser<&'a str, Output = String> {
    combine::satisfy::<&str, _>(|c| c.is_alphabetic()).then(|first: char| {
        combine::parser::<&str, String, _>(move |input| {
            let mut buffer = String::new();
            buffer.push(first);
            let mut iterator =
                combine::satisfy(|c: char| c.is_alphanumeric() || c == '.').iter(input);
            buffer.extend(&mut iterator);
            iterator.into_result(buffer.clone())
        })
    })
}

fn literal_integer_separator<'a>() -> impl Parser<&'a str> {
    combine::skip_many(char::char('_'))
}

fn literal_integer_digits<'a, D: Parser<&'a str, Output = char>>(
    radix: u32,
    digit_parser: D,
) -> impl Parser<&'a str, Output = i128> {
    combine::sep_by1::<String, _, D, _>(digit_parser, combine::skip_many(char::char('_')))
        .map(move |digits: String| i128::from_str_radix(&digits, radix).unwrap())
}

fn literal_integer<'a>() -> impl Parser<&'a str, Output = i128> {
    combine::choice((
        char::string("0x").with(literal_integer_digits(16, char::hex_digit())),
        char::string("0b").with(literal_integer_digits(2, combine::one_of("01".chars()))),
        (
            combine::optional(char::char('-')).map(|neg| neg.is_some()),
            literal_integer_digits(10, char::digit()),
        )
            .map(|(is_negative, value)| if is_negative { value * -1 } else { value }),
    ))
}

fn character_token<'a>(c: char, token: Token) -> impl Parser<&'a str, Output = Token> {
    char::char(c).with(combine::value(token))
}

fn token<'a>() -> impl Parser<&'a str, Output = Token> {
    combine::choice((
        directive().map(Token::Directive),
        keyword().map(Token::Keyword),
        literal_integer().map(Token::LiteralInteger),
        character_token(';', Token::Semicolon),
        character_token('{', Token::OpenBracket),
        character_token('}', Token::CloseBracket),
        character_token('(', Token::OpenParenthesis),
        character_token(')', Token::CloseParenthesis),
        combine::any().with(combine::value(Token::Unknown)), // TODO: To reduce memory usage, try to maximize number of unknown chars parsed.
    ))
}

fn positioned_token<'a>() -> impl Parser<&'a str, Output = PositionedToken> {
    (combine::position(),
    token())
    .map(|(position, token)| PositionedToken { token,  })
}

pub fn lex(input: &str) -> Vec<Token> {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use crate::lexer::{lex, Token};

    #[test]
    fn basic_sequence_test() {
        assert_eq!(
            lex("{ret;42"),
            vec![
                Token::OpenBracket,
                Token::Keyword(String::from("ret")),
                Token::Semicolon,
                Token::LiteralInteger(42),
            ]
        );
    }

    #[test]
    fn format_directive_tokens_test() {
        assert_eq!(
            lex(".format { .major 0; .minor 0x1; }"),
            vec![
                Token::Directive(String::from("format")),
                Token::OpenBracket,
                Token::Directive(String::from("major")),
                Token::LiteralInteger(0),
                Token::Semicolon,
                Token::Directive(String::from("minor")),
                Token::LiteralInteger(1),
                Token::Semicolon,
                Token::CloseBracket,
            ]
        );
    }
}
