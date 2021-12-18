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
    LiteralString(ast::LiteralString),
    /// An instruction or other keyword.
    Keyword(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

type ParserInput<'a> = combine::stream::easy::Stream<
    combine::stream::position::Stream<&'a str, combine::stream::position::SourcePosition>,
>;

fn skip_parser<'a, P: Parser<ParserInput<'a>>>(p: P) -> impl Parser<ParserInput<'a>, Output = ()> {
    p.map(|_| ())
}

fn period<'a>() -> impl Parser<ParserInput<'a>> {
    char::char('.')
}

fn newline<'a>() -> impl Parser<ParserInput<'a>, Output = ()> {
    combine::choice((
        skip_parser(char::crlf()),
        skip_parser(char::newline()),
        combine::eof(),
    ))
    .expected("new line or end of file")
}

fn whitespace_or_comments<'a>() -> impl Parser<ParserInput<'a>, Output = ()> {
    combine::choice((
        char::string("//")
            .with(combine::skip_many1(combine::not_followed_by(
                // Hack to get () returning parser to work with not_followed_by
                newline().map(|()| '\n'),
            )))
            .expected("single-line comment"),
        char::spaces(),
    ))
}

fn directive<'a>() -> impl Parser<ParserInput<'a>, Output = String> {
    period()
        .with(combine::many1::<String, _, _>(char::alpha_num()))
        .expected("directive")
}

fn keyword<'a>() -> impl Parser<ParserInput<'a>, Output = String> {
    combine::satisfy::<ParserInput<'a>, _>(|c| c.is_alphabetic())
        .then(|first: char| {
            combine::parser::<ParserInput<'a>, String, _>(move |input| {
                let mut buffer = String::new();
                buffer.push(first);
                let mut iterator =
                    combine::satisfy(|c: char| c.is_alphanumeric() || c == '.').iter(input);
                buffer.extend(&mut iterator);
                iterator.into_result(buffer.clone())
            })
        })
        .expected("keyword")
}

fn literal_integer_digits<'a, D: Parser<ParserInput<'a>, Output = char>>(
    radix: u32,
    digit_parser: D,
) -> impl Parser<ParserInput<'a>, Output = i128> {
    combine::sep_by1::<String, _, _, _>(digit_parser, combine::skip_many(char::char('_')))
        // Probably safe to unwrap, digits are guaranteed to be correct.
        .map(move |digits: String| i128::from_str_radix(&digits, radix).unwrap())
}

fn literal_integer<'a>() -> impl Parser<ParserInput<'a>, Output = i128> {
    combine::choice((
        combine::attempt(char::string("0x").with(literal_integer_digits(16, char::hex_digit()))),
        combine::attempt(
            char::string("0b").with(literal_integer_digits(2, combine::one_of("01".chars()))),
        ),
        (
            combine::optional(char::char('-')).map(|neg| neg.is_some()),
            literal_integer_digits(10, char::digit()),
        )
            .map(|(is_negative, value)| if is_negative { -value } else { value }),
    ))
    .expected("integer literal")
}

// TODO: Allow escape sequences in literal strings.
fn literal_string<'a>() -> impl Parser<ParserInput<'a>, Output = ast::LiteralString> {
    combine::between(
        char::char('\"'),
        char::char('\"'),
        combine::many::<Vec<char>, _, _>(combine::satisfy(|c: char| c != '\"')),
    )
    .map(ast::LiteralString)
}

fn character_token<'a>(c: char, token: Token) -> impl Parser<ParserInput<'a>, Output = Token> {
    char::char(c).with(combine::value(token))
}

fn token<'a>() -> impl Parser<ParserInput<'a>, Output = Token> {
    combine::choice((
        directive().map(Token::Directive),
        keyword().map(Token::Keyword),
        literal_integer().map(Token::LiteralInteger),
        literal_string().map(Token::LiteralString),
        character_token(';', Token::Semicolon),
        character_token('{', Token::OpenBracket),
        character_token('}', Token::CloseBracket),
        character_token('(', Token::OpenParenthesis),
        character_token(')', Token::CloseParenthesis),
        combine::any().with(combine::value(Token::Unknown)), // TODO: To reduce memory usage, try to maximize number of unknown chars parsed.
    ))
    .expected("token")
}

fn positioned_token<'a>() -> impl Parser<ParserInput<'a>, Output = PositionedToken> {
    (combine::position(), token()).map(|(position, token)| {
        PositionedToken::new(
            token,
            (position.line - 1) as u32,
            (position.column - 1) as u32,
        )
    })
}

fn positioned_token_sequence<'a>() -> impl Parser<ParserInput<'a>, Output = Vec<PositionedToken>> {
    whitespace_or_comments()
        .with(combine::sep_by::<Vec<_>, _, _, _>(
            positioned_token(),
            whitespace_or_comments(),
        ))
        .skip(combine::eof())
}

fn lexer_input(input: &str) -> ParserInput<'_> {
    combine::stream::easy::Stream(combine::stream::position::Stream::new(input))
}

pub fn lex(input: &str) -> Vec<PositionedToken> {
    match positioned_token_sequence().parse(lexer_input(input)) {
        Ok((tokens, _)) => tokens,
        Err(error) => panic!("{}", error),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast;
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
            lex(".format {\n  .major 0;\n  .minor 0x1_0;\n}"),
            vec![
                PositionedToken::new(Token::Directive(String::from("format")), 0, 0),
                PositionedToken::new(Token::OpenBracket, 0, 8),
                PositionedToken::new(Token::Directive(String::from("major")), 1, 2),
                PositionedToken::new(Token::LiteralInteger(0), 1, 9),
                PositionedToken::new(Token::Semicolon, 1, 10),
                PositionedToken::new(Token::Directive(String::from("minor")), 2, 2),
                PositionedToken::new(Token::LiteralInteger(16), 2, 9),
                PositionedToken::new(Token::Semicolon, 2, 14),
                PositionedToken::new(Token::CloseBracket, 3, 0),
            ]
        );
    }

    #[test]
    fn module_directive_tokens_test() {
        assert_eq!(
            lex(".module { .name \"Hello\" };"),
            vec![
                PositionedToken::new(Token::Directive(String::from("module")), 0, 0),
                PositionedToken::new(Token::OpenBracket, 0, 8),
                PositionedToken::new(Token::Directive(String::from("name")), 0, 10),
                PositionedToken::new(
                    Token::LiteralString(ast::LiteralString::from("Hello")),
                    0,
                    16
                ),
                PositionedToken::new(Token::CloseBracket, 0, 24),
                PositionedToken::new(Token::Semicolon, 0, 25),
            ]
        );
    }
}
