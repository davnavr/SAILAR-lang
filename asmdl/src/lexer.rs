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

#[derive(Debug, Eq, PartialEq)]
pub struct PositionedToken {
    pub token: Token,
    pub position: ast::Position,
}

impl PositionedToken {
    pub fn new(token: Token, line: u32, column: u32) -> PositionedToken {
        Self {
            token,
            position: ast::Position { line, column },
        }
    }
}

type TokenInput<'a> =
    combine::stream::position::Stream<&'a str, combine::stream::position::SourcePosition>;

fn period<'a>() -> impl Parser<TokenInput<'a>> {
    char::char('.')
}

fn newline<'a>() -> impl Parser<TokenInput<'a>> {
    combine::choice((char::crlf(), char::newline()))
}

fn whitespace_or_comments<'a>() -> impl Parser<TokenInput<'a>> {
    combine::choice((newline(), newline()))
}

fn directive<'a>() -> impl Parser<TokenInput<'a>, Output = String> {
    period().with(combine::many1::<String, _, _>(char::alpha_num()))
}

fn keyword<'a>() -> impl Parser<TokenInput<'a>, Output = String> {
    combine::satisfy::<TokenInput<'a>, _>(|c| c.is_alphabetic()).then(|first: char| {
        combine::parser::<TokenInput<'a>, String, _>(move |input| {
            let mut buffer = String::new();
            buffer.push(first);
            let mut iterator =
                combine::satisfy(|c: char| c.is_alphanumeric() || c == '.').iter(input);
            buffer.extend(&mut iterator);
            iterator.into_result(buffer.clone())
        })
    })
}

fn literal_integer_digits<'a, D: Parser<TokenInput<'a>, Output = char>>(
    radix: u32,
    digit_parser: D,
) -> impl Parser<TokenInput<'a>, Output = i128> {
    combine::sep_by1::<String, _, D, _>(digit_parser, combine::skip_many(char::char('_')))
        .map(move |digits: String| i128::from_str_radix(&digits, radix).unwrap())
}

fn literal_integer<'a>() -> impl Parser<TokenInput<'a>, Output = i128> {
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

fn character_token<'a>(c: char, token: Token) -> impl Parser<TokenInput<'a>, Output = Token> {
    char::char(c).with(combine::value(token))
}

fn token<'a>() -> impl Parser<TokenInput<'a>, Output = Token> {
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

fn positioned_token<'a>() -> impl Parser<TokenInput<'a>, Output = PositionedToken> {
    (combine::position(), token()).map(|(position, token)| {
        PositionedToken::new(token, position.line as u32, position.column as u32)
    })
}

fn positioned_token_sequence<'a>() -> impl Parser<TokenInput<'a>, Output = Vec<PositionedToken>> {
    whitespace_or_comments().with(combine::sep_by1::<Vec<_>, _, _, _>(
        positioned_token(),
        whitespace_or_comments(),
    ))
}

pub fn lex(input: &str) -> Vec<PositionedToken> {
    positioned_token_sequence()
        .parse(TokenInput::new(input))
        .unwrap()
        .0
}

#[cfg(test)]
mod tests {
    use crate::lexer::{lex, PositionedToken, Token};

    #[test]
    fn basic_sequence_test() {
        assert_eq!(
            lex("{ret;42"),
            vec![
                PositionedToken::new(Token::OpenBracket, 0, 0),
                PositionedToken::new(Token::Keyword(String::from("ret")), 0, 1),
                PositionedToken::new(Token::Semicolon, 0, 4),
                PositionedToken::new(Token::LiteralInteger(42), 0, 5),
            ]
        );
    }

    #[test]
    fn format_directive_tokens_test() {
        assert_eq!(
            lex(".format {\n  .major 0;\n  .minor 0x1;\n}"),
            vec![
                PositionedToken::new(Token::Directive(String::from("format")), 0, 0),
                PositionedToken::new(Token::OpenBracket, 0, 8),
                PositionedToken::new(Token::Directive(String::from("major")), 1, 2),
                PositionedToken::new(Token::LiteralInteger(0), 1, 9),
                PositionedToken::new(Token::Semicolon, 1, 10),
                PositionedToken::new(Token::Directive(String::from("minor")), 2, 2),
                PositionedToken::new(Token::LiteralInteger(1), 2, 9),
                PositionedToken::new(Token::Semicolon, 2, 10),
                PositionedToken::new(Token::CloseBracket, 3, 0),
            ]
        );
    }
}
