use crate::{ast, lexer};
use combine::{stream::easy, stream::StreamErrorFor, Parser};

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    InvalidFormatVersion(i128),
    ParseFailed(Vec<easy::Error<Box<lexer::Token>, Box<lexer::Token>>>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidFormatVersion(value) => write!(
                f,
                "{} is not a valid format version, since it cannot be represented in a single byte",
                value
            ),
            Self::ParseFailed(error) => {
                for e in error {
                    std::fmt::Display::fmt(e, f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct PositionedParserError {
    pub position: Option<ast::Position>,
    pub error: Error,
}

#[derive(Clone, Debug)]
pub struct TokenStream<'a> {
    tokens: &'a [lexer::PositionedToken],
}

impl<'a> combine::StreamOnce for TokenStream<'a> {
    type Token = &'a lexer::PositionedToken;
    type Range = Self::Token;
    type Position = Option<ast::Position>;
    type Error = easy::Errors<Self::Token, Self::Range, Self::Position>;

    fn uncons(&mut self) -> Result<Self::Token, StreamErrorFor<Self>> {
        match self.tokens.split_first() {
            Some((parsed, remaining)) => {
                self.tokens = remaining;
                Ok(parsed)
            }
            None => Err(easy::Error::end_of_input()),
        }
    }
}

impl<'a> combine::Positioned for TokenStream<'a> {
    fn position(&self) -> Self::Position {
        self.tokens.first().map(|token| token.position)
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

type ParserInput<'a> = combine::stream::state::Stream<TokenStream<'a>, Vec<PositionedParserError>>;

fn expect_token<'a>(
    expected: lexer::Token,
) -> impl Parser<ParserInput<'a>, Output = &'a lexer::PositionedToken> {
    combine::satisfy(move |token: &lexer::PositionedToken| token.token == expected)
}

fn positioned<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = ast::Positioned<T>> {
    (combine::position(), parser).and_then(|(position, value)| match position {
        Some(position) => Ok(ast::Positioned { value, position }),
        None => Err(easy::Error::end_of_input()),
    })
}

fn global_symbol<'a>() -> impl Parser<ParserInput<'a>, Output = ast::GlobalSymbol> {
    combine::satisfy_map(|token: &lexer::PositionedToken| match &token.token {
        lexer::Token::GlobalIdentifier(id) => Some(ast::GlobalSymbol(ast::Positioned {
            position: token.position,
            value: id.clone(),
        })),
        _ => None,
    })
}

fn local_symbol<'a>() -> impl Parser<ParserInput<'a>, Output = ast::LocalSymbol> {
    combine::satisfy_map(|token: &lexer::PositionedToken| match &token.token {
        lexer::Token::LocalIdentifier(id) => Some(ast::LocalSymbol(ast::Positioned {
            position: token.position,
            value: id.clone(),
        })),
        _ => None,
    })
}

fn register_symbol<'a>() -> impl Parser<ParserInput<'a>, Output = ast::RegisterSymbol> {
    combine::satisfy_map(|token: &lexer::PositionedToken| match &token.token {
        lexer::Token::RegisterIdentifier(id) => Some(ast::RegisterSymbol(ast::Positioned {
            position: token.position,
            value: id.clone(),
        })),
        _ => None,
    })
}

fn keyword<'a>(
    name: &'static str,
) -> impl Parser<ParserInput<'a>, Output = &'a lexer::PositionedToken> {
    expect_token(lexer::Token::Keyword(String::from(name)))
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

fn literal_integer_sized<'a, T: TryFrom<i128, Error = std::num::TryFromIntError>>(
) -> impl Parser<ParserInput<'a>, Output = T> {
    literal_integer().and_then(|value: i128| {
        T::try_from(value).map_err(|error| easy::Error::Other(Box::new(error)))
    })
}

fn literal_string<'a>() -> impl Parser<ParserInput<'a>, Output = ast::LiteralString> {
    combine::satisfy_map(|token: &'a lexer::PositionedToken| match &token.token {
        lexer::Token::LiteralString(value) => Some(value.clone()),
        _ => None,
    })
}

fn between_tokens<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
    open: lexer::Token,
    close: lexer::Token,
) -> impl Parser<ParserInput<'a>, Output = T> {
    combine::between(expect_token(open), expect_token(close), parser)
}

fn between_brackets<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = T> {
    between_tokens(
        parser,
        lexer::Token::OpenBracket,
        lexer::Token::CloseBracket,
    )
}

fn between_parenthesis<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = T> {
    between_tokens(
        parser,
        lexer::Token::OpenParenthesis,
        lexer::Token::CloseParenthesis,
    )
}

fn format_version<'a>(
    name: &'static str,
) -> impl Parser<ParserInput<'a>, Output = registir::format::uvarint> {
    directive(name, literal_integer_sized::<u64>()).map(registir::format::uvarint)
}

fn format_declaration<'a>() -> impl combine::Parser<ParserInput<'a>, Output = ast::FormatDeclaration>
{
    combine::choice((
        format_version("major").map(ast::FormatDeclaration::Major),
        format_version("minor").map(ast::FormatDeclaration::Minor),
    ))
}

fn name_directive<'a, D, N: Fn(ast::Positioned<ast::LiteralString>) -> D>(
    name_mapper: N,
) -> impl Parser<ParserInput<'a>, Output = D> {
    directive("name", positioned(literal_string())).map(name_mapper)
}

fn module_declaration<'a>() -> impl Parser<ParserInput<'a>, Output = ast::ModuleDeclaration> {
    combine::choice((
        name_directive(ast::ModuleDeclaration::Name),
        directive(
            "version",
            combine::many::<Vec<_>, _, _>(literal_integer_sized::<u64>()),
        )
        .map(ast::ModuleDeclaration::Version),
    ))
}

fn data_declaration<'a>() -> impl Parser<ParserInput<'a>, Output = ast::DataKind> {
    combine::choice((
        keyword("bytes").map(|_| unimplemented!()),
        keyword("string").with(literal_string().map(|content| ast::DataKind::String { content })),
    ))
}

fn primitive_type<'a>() -> impl Parser<ParserInput<'a>, Output = ast::PrimitiveType> {
    combine::choice((
        keyword("u32").with(combine::value(ast::PrimitiveType::U32)),
        keyword("s32").with(combine::value(ast::PrimitiveType::S32)),
    ))
}

fn type_signature<'a>() -> impl Parser<ParserInput<'a>, Output = ast::TypeSignature> {
    combine::choice((primitive_type().map(ast::TypeSignature::Primitive),))
}

fn code_statement<'a>() -> impl Parser<ParserInput<'a>, Output = ast::Statement> {
    (
        combine::optional(
            combine::sep_by1::<Vec<_>, _, _, _>(
                register_symbol(),
                expect_token(lexer::Token::Comma),
            )
            .skip(expect_token(lexer::Token::Equals)),
        )
        .map(Option::unwrap_or_default),
        positioned(combine::choice((
            keyword("nop").with(combine::value(ast::Instruction::Nop)),
            keyword("ret").with(
                combine::sep_by(register_symbol(), expect_token(lexer::Token::Comma))
                    .map(ast::Instruction::Ret),
            ),
            keyword("const.zero").with(primitive_type().map(ast::Instruction::ConstZero)),
        ))),
    )
        .map(|(registers, instruction)| ast::Statement {
            registers,
            instruction,
        })
        .skip(expect_token(lexer::Token::Semicolon))
}

fn code_declaration<'a>() -> impl Parser<ParserInput<'a>, Output = ast::CodeDeclaration> {
    combine::choice((
        directive("entry", local_symbol()).map(ast::CodeDeclaration::Entry),
        directive(
            "block",
            (
                local_symbol(),
                combine::optional(between_parenthesis(combine::sep_by::<Vec<_>, _, _, _>(
                    register_symbol(),
                    expect_token(lexer::Token::Comma),
                ))),
                between_brackets(combine::many(code_statement())),
            ),
        )
        .map(
            |(name, arguments, instructions)| ast::CodeDeclaration::Block {
                name,
                arguments: arguments.unwrap_or_default(),
                instructions,
            },
        ),
    ))
}

fn type_modifier<'a>() -> impl Parser<ParserInput<'a>, Output = ast::TypeModifier> {
    combine::choice((
        keyword("public").with(combine::value(ast::TypeModifier::Public)),
        keyword("private").with(combine::value(ast::TypeModifier::Private)),
    ))
}

fn method_types<'a>(
) -> impl Parser<ParserInput<'a>, Output = Vec<ast::Positioned<ast::TypeSignature>>> {
    between_parenthesis(combine::sep_by(
        positioned(type_signature()),
        expect_token(lexer::Token::Comma),
    ))
}

fn method_modifier<'a>() -> impl Parser<ParserInput<'a>, Output = ast::MethodModifier> {
    combine::choice((
        keyword("public").with(combine::value(ast::MethodModifier::Public)),
        keyword("private").with(combine::value(ast::MethodModifier::Private)),
        keyword("instance").with(combine::value(ast::MethodModifier::Instance)),
        keyword("initializer").with(combine::value(ast::MethodModifier::Initializer)),
    ))
}

fn method_declaration<'a>() -> impl Parser<ParserInput<'a>, Output = ast::MethodDeclaration> {
    combine::choice((
        name_directive(ast::MethodDeclaration::Name),
        directive(
            "body",
            combine::choice((keyword("defined")
                .with(global_symbol())
                .map(ast::MethodBodyDeclaration::Defined),)),
        )
        .map(ast::MethodDeclaration::Body),
    ))
}

fn type_declaration<'a>() -> impl Parser<ParserInput<'a>, Output = ast::TypeDeclaration> {
    combine::choice((
        name_directive(ast::TypeDeclaration::Name),
        directive("namespace", combine::many(positioned(literal_string())))
            .map(ast::TypeDeclaration::Namespace),
        directive(
            "method",
            (
                global_symbol(),
                method_types(),
                combine::optional(keyword("returns").with(method_types()))
                    .map(Option::unwrap_or_default),
                combine::many::<Vec<_>, _, _>(positioned(method_modifier())),
                declaration_block(method_declaration()),
            ),
        )
        .map(
            |(symbol, parameter_types, return_types, modifiers, declarations)| {
                ast::TypeDeclaration::Method {
                    symbol,
                    parameter_types,
                    return_types,
                    modifiers,
                    declarations,
                }
            },
        ),
    ))
}

fn declaration_block<'a, T, P: Parser<ParserInput<'a>, Output = T>>(
    parser: P,
) -> impl Parser<ParserInput<'a>, Output = Vec<ast::Positioned<T>>> {
    between_brackets(combine::many(positioned(parser)))
}

fn top_level_declaration<'a>(
) -> impl combine::Parser<ParserInput<'a>, Output = ast::TopLevelDeclaration> {
    combine::choice((
        directive("format", declaration_block(format_declaration()))
            .map(ast::TopLevelDeclaration::Format),
        directive("module", declaration_block(module_declaration()))
            .map(ast::TopLevelDeclaration::Module),
        directive(
            "data",
            (global_symbol(), data_declaration())
                .map(|(symbol, kind)| ast::TopLevelDeclaration::Data { symbol, kind }),
        ),
        directive(
            "code",
            (global_symbol(), declaration_block(code_declaration())).map(
                |(symbol, declarations)| ast::TopLevelDeclaration::Code {
                    symbol,
                    declarations,
                },
            ),
        ),
        directive(
            "type",
            (
                global_symbol(),
                combine::many::<Vec<_>, _, _>(positioned(type_modifier())),
                declaration_block(type_declaration()),
            )
                .map(|(symbol, modifiers, declarations)| {
                    ast::TopLevelDeclaration::Type {
                        symbol,
                        modifiers,
                        declarations,
                    }
                }),
        ),
    ))
}

pub type ParseResult =
    Result<Vec<ast::Positioned<ast::TopLevelDeclaration>>, Vec<PositionedParserError>>;

pub fn parse_tokens(tokens: &[lexer::PositionedToken]) -> ParseResult {
    match combine::many::<Vec<_>, _, _>(positioned(top_level_declaration())).parse(
        combine::stream::state::Stream {
            stream: TokenStream { tokens },
            state: Vec::new(),
        },
    ) {
        Ok((declarations, _)) => Ok(declarations),
        // A good parser should return more than one error, but for now, this is good enough.
        Err(error) => Err(vec![PositionedParserError {
            error: Error::ParseFailed(
                error
                    .errors
                    .into_iter()
                    .map(|e| {
                        e.map_token(|token| Box::new(token.token.clone()))
                            .map_range(|token| Box::new(token.token.clone()))
                    })
                    .collect::<Vec<_>>(),
            ),
            position: error.position,
        }]),
    }
}

pub fn parse(declarations: &str) -> ParseResult {
    parse_tokens(&lexer::lex(declarations))
}

#[cfg(test)]
mod tests {
    use crate::{ast, parser};

    #[test]
    fn format_declaration_test() {
        assert_eq!(
            parser::parse(".format { .major 0; .minor 1; };").unwrap(),
            vec![ast::Positioned::new(
                0,
                0,
                ast::TopLevelDeclaration::Format(vec![
                    ast::Positioned::new(
                        0,
                        10,
                        ast::FormatDeclaration::Major(registir::format::uvarint(0))
                    ),
                    ast::Positioned::new(
                        0,
                        20,
                        ast::FormatDeclaration::Minor(registir::format::uvarint(1))
                    )
                ])
            )]
        )
    }

    #[test]
    fn module_declaration_test() {
        assert_eq!(
            parser::parse(".module {\n    .name \"Hey\"; .version 1 0 0;\n};").unwrap(),
            vec![ast::Positioned::new(
                0,
                0,
                ast::TopLevelDeclaration::Module(vec![
                    ast::Positioned::new(
                        1,
                        4,
                        ast::ModuleDeclaration::Name(ast::Positioned::new(
                            1,
                            10,
                            ast::LiteralString::from("Hey")
                        ))
                    ),
                    ast::Positioned::new(1, 17, ast::ModuleDeclaration::Version(vec![1, 0, 0]))
                ])
            )]
        )
    }
}
