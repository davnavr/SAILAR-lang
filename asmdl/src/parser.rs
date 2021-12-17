use crate::{ast, lexer};
use combine::{stream::easy, stream::StreamErrorFor, Parser};

#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum ParserError {
    InvalidFormatVersion(i128),
}

#[derive(Debug, PartialEq)]
pub struct PositionedParserError {
    pub position: Option<ast::Position>,
    pub error: ParserError,
}

#[derive(Clone, Debug)]
pub struct TokenStream<'a> {
    tokens: &'a [lexer::PositionedToken],
}

impl<'a> combine::StreamOnce for TokenStream<'a> {
    type Token = &'a lexer::PositionedToken;
    type Range = &'a [lexer::PositionedToken];
    type Position = Option<ast::Position>;
    type Error = easy::Errors<Self::Token, Self::Range, Self::Position>;

    fn uncons(&mut self) -> Result<Self::Token, StreamErrorFor<Self>> {
        match self.tokens.split_first() {
            Some((ref parsed, remaining)) => {
                self.tokens = remaining;
                Ok(parsed)
            }
            None => Err(easy::Error::end_of_input()),
        }
    }
}

impl<'a> combine::Positioned for TokenStream<'a> {
    fn position(&self) -> Self::Position {
        self.tokens.first().map(|token| token.position.clone())
    }
}

impl<'a> combine::stream::ResetStream for TokenStream<'a> {
    type Checkpoint = &'a [lexer::PositionedToken];

    fn checkpoint(&self) -> Self::Checkpoint {
        combine::stream::ResetStream::checkpoint(&self.tokens)
    }

    fn reset(&mut self, checkpoint: Self::Checkpoint) -> Result<(), Self::Error> {
        combine::stream::ResetStream::reset(&mut self.tokens, checkpoint)
            .map_err(|_| unimplemented!())
    }
}

type ParserInput<'a> = combine::stream::state::Stream<TokenStream<'a>, Vec<ParserError>>;

fn expect_token<'a>(
    expected: lexer::Token,
) -> impl Parser<ParserInput<'a>, Output = &'a lexer::PositionedToken> {
    combine::satisfy(move |token: &lexer::PositionedToken| token.token == expected)
}

fn directive<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    name: &'static str,
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = T> {
    expect_token(lexer::Token::Directive(String::from(name)))
        .with(parser)
        .skip(expect_token(lexer::Token::Semicolon))
}

fn literal_integer<'a>() -> impl Parser<ParserInput<'a>, Output = i128> {
    combine::satisfy_map(|token: &'a lexer::PositionedToken| match token.token {
        lexer::Token::LiteralInteger(value) => Some(value),
        _ => None,
    })
}

fn literal_integer_sized<
    'a,
    T: TryFrom<i128>,
    ErrMap: FnMut(T::Error) -> easy::Error<&'a lexer::PositionedToken, &'a [lexer::PositionedToken]>,
>(
    error_mapper: ErrMap,
) -> impl Parser<ParserInput<'a>, Output = T>
where
    T::Error: std::error::Error + Send + Sync,
{
    literal_integer().and_then(|value: i128| T::try_from(value).map_err(error_mapper))
}

fn literal_string<'a>() -> impl Parser<ParserInput<'a>, Output = ast::LiteralString> {
    combine::satisfy_map(|token: &'a lexer::PositionedToken| match token.token {
        lexer::Token::LiteralString(value) => Some(value),
        _ => None,
    })
}

fn between_brackets<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = T> {
    combine::between(
        expect_token(lexer::Token::OpenBracket),
        expect_token(lexer::Token::CloseBracket),
        parser,
    )
}

fn format_version<'a>(name: &'static str) -> impl Parser<ParserInput<'a>, Output = u8> {
    directive(
        name,
        literal_integer_sized::<u8, _>(|error| easy::Error::Other(Box::new(error))),
    )
}

fn format_declaration<'a>() -> impl combine::Parser<ParserInput<'a>, Output = ast::FormatDeclaration>
{
    combine::choice((
        format_version("major").map(ast::FormatDeclaration::Major),
        format_version("minor").map(ast::FormatDeclaration::Minor),
    ))
}

fn module_declaration<'a>() -> impl combine::Parser<ParserInput<'a>, Output = ast::ModuleDeclaration>
{
    combine::choice((
        directive("name", literal_string()).map(ast::ModuleDeclaration::Name),
        directive(
            "version",
            combine::many::<Vec<_>, _, _>(literal_integer_sized::<u64, _>(|error| {
                easy::Error::Other(Box::new(error))
            })),
        )
        .map(ast::ModuleDeclaration::Version),
    ))
}

fn top_level_declaration<'a>() -> impl combine::Parser<ParserInput<'a>, Output = ast::TopLevelDeclaration> {
    combine::choice((
        directive(
            "format",
            between_brackets(combine::many::<Vec<_>, _, _>(format_declaration())),
        )
        .map(ast::TopLevelDeclaration::Format),
        directive(
            "module",
            between_brackets(combine::many::<Vec<_>, _, _>(module_declaration())),
        )
        .map(ast::TopLevelDeclaration::Module),
    ))
}
