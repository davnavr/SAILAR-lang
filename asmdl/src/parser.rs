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
) -> impl Parser<ParserInput<'a>, Output = registir::format::numeric::UInteger> {
    directive(name, literal_integer_sized::<u32>()).map(registir::format::numeric::UInteger)
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
            combine::many::<Vec<_>, _, _>(literal_integer_sized::<u32>()),
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
        keyword("u8").with(combine::value(ast::PrimitiveType::U8)),
        keyword("s8").with(combine::value(ast::PrimitiveType::S8)),
        keyword("u16").with(combine::value(ast::PrimitiveType::U16)),
        keyword("s16").with(combine::value(ast::PrimitiveType::S16)),
        keyword("u32").with(combine::value(ast::PrimitiveType::U32)),
        keyword("s32").with(combine::value(ast::PrimitiveType::S32)),
        keyword("u64").with(combine::value(ast::PrimitiveType::U64)),
        keyword("s64").with(combine::value(ast::PrimitiveType::S64)),
    ))
}

fn type_signature<'a>() -> impl Parser<ParserInput<'a>, Output = ast::TypeSignature> {
    combine::choice((primitive_type().map(ast::TypeSignature::Primitive),))
}

fn numeric_type<'a>() -> impl Parser<ParserInput<'a>, Output = ast::NumericType> {
    primitive_type().map(ast::NumericType::Primitive)
}

fn overflow_modifier<'a>(
) -> impl Parser<ParserInput<'a>, Output = Option<ast::Positioned<ast::OverflowModifier>>> {
    combine::optional(positioned(combine::choice((
        keyword("ovf.halt").with(combine::value(ast::OverflowModifier::Halt)),
        keyword("ovf.flag").with(combine::value(ast::OverflowModifier::Flag)),
    ))))
}

fn basic_arithmetic_operation<'a, I: 'a + Fn(ast::BasicArithmeticOperation) -> ast::Instruction>(
    name: &'static str,
    separator: &'static str,
    instruction: I,
) -> impl Parser<ParserInput<'a>, Output = ast::Instruction> {
    keyword(name)
        .with((
            positioned(numeric_type()),
            register_symbol(),
            keyword(separator).with(register_symbol()),
            overflow_modifier(),
        ))
        .map(move |(return_type, x, y, overflow_modifier)| {
            instruction(ast::BasicArithmeticOperation {
                return_type,
                x,
                y,
                overflow_modifier,
            })
        })
}

fn division_operation<'a, I: 'a + Fn(ast::DivisionOperation) -> ast::Instruction>(
    name: &'static str,
    instruction: I,
) -> impl Parser<ParserInput<'a>, Output = ast::Instruction> {
    keyword(name)
        .with((
            positioned(numeric_type()),
            register_symbol(),
            keyword("over").with(register_symbol()),
            combine::choice((
                keyword("or")
                    .with(register_symbol())
                    .map(ast::DivideByZeroModifier::Return),
                keyword("zeroed.halt").with(combine::value(ast::DivideByZeroModifier::Halt)),
            )),
            overflow_modifier(),
        ))
        .map(
            move |(
                return_type,
                numerator,
                denominator,
                divide_by_zero_modifier,
                overflow_modifier,
            )| {
                instruction(ast::DivisionOperation {
                    return_type,
                    numerator,
                    denominator,
                    divide_by_zero_modifier,
                    overflow_modifier,
                })
            },
        )
}

fn bitwise_operation<'a, I: 'a + Fn(ast::BitwiseOperation) -> ast::Instruction>(
    name: &'static str,
    separator: Option<&'static str>,
    instruction: I,
) -> impl Parser<ParserInput<'a>, Output = ast::Instruction> {
    keyword(name)
        .with((
            positioned(numeric_type()),
            match separator {
                Some(separator) => combine::parser::combinator::Either::Left(
                    register_symbol().skip(keyword(separator)),
                ),
                None => combine::parser::combinator::Either::Right(register_symbol()),
            },
            register_symbol(),
        ))
        .map(move |(result_type, x, y)| instruction(ast::BitwiseOperation { result_type, x, y }))
}

fn bitwise_shift_operation<'a, I: 'a + Fn(ast::BitwiseOperation) -> ast::Instruction>(
    name: &'static str,
    instruction: I,
) -> impl Parser<ParserInput<'a>, Output = ast::Instruction> {
    bitwise_operation(name, Some("by"), instruction)
}

fn many_register_symbols<'a>() -> impl Parser<ParserInput<'a>, Output = Vec<ast::RegisterSymbol>> {
    combine::sep_by(register_symbol(), expect_token(lexer::Token::Comma))
}

fn input_register_symbols<'a>() -> impl Parser<ParserInput<'a>, Output = Vec<ast::RegisterSymbol>> {
    combine::optional(keyword("with").with(many_register_symbols())).map(Option::unwrap_or_default)
}

fn tail_call_behavior<'a>() -> impl Parser<ParserInput<'a>, Output = ast::TailCall> {
    combine::choice((
        keyword("tail.required").with(combine::value(ast::TailCall::Required)),
        keyword("tail.prohibited").with(combine::value(ast::TailCall::Prohibited)),
        combine::value(ast::TailCall::Allowed),
    ))
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
            combine::choice((
                keyword("nop").with(combine::value(ast::Instruction::Nop)),
                keyword("ret").with(many_register_symbols().map(ast::Instruction::Ret)),
                keyword("call")
                    .with((
                        tail_call_behavior(),
                        global_symbol(),
                        many_register_symbols(),
                    ))
                    .map(|(tail_call, method, arguments)| ast::Instruction::Call {
                        tail_call,
                        method,
                        arguments,
                    }),
            )),
            // Branch instructions
            combine::choice((
                keyword("br")
                    .with((local_symbol(), input_register_symbols()))
                    .map(|(target, input_registers)| ast::Instruction::Br(target, input_registers)),
                keyword("br.if")
                    .with((
                        register_symbol(),
                        keyword("then").with(local_symbol()),
                        keyword("else").with(local_symbol()),
                        input_register_symbols(),
                    ))
                    .map(|(condition, true_branch, false_branch, input_registers)| {
                        ast::Instruction::BrIf {
                            condition,
                            true_branch,
                            false_branch,
                            input_registers,
                        }
                    }),
            )),
            // Arithmetic instructions
            combine::choice((
                basic_arithmetic_operation("add", "and", ast::Instruction::Add),
                basic_arithmetic_operation("sub", "from", ast::Instruction::Sub),
                basic_arithmetic_operation("mul", "by", ast::Instruction::Mul),
                division_operation("div", ast::Instruction::Div),
            )),
            // Bitwise instructions
            combine::choice((
                bitwise_operation("and", None, ast::Instruction::And),
                bitwise_operation("or", None, ast::Instruction::Or),
                keyword("not")
                    .with((positioned(numeric_type()), register_symbol()))
                    .map(move |(result_type, value)| ast::Instruction::Not(result_type, value)),
                bitwise_operation("xor", None, ast::Instruction::Xor),
                bitwise_shift_operation("sh.l", ast::Instruction::ShL),
                bitwise_shift_operation("sh.r", ast::Instruction::ShR),
                bitwise_shift_operation("rot.l", ast::Instruction::RotL),
                bitwise_shift_operation("rot.r", ast::Instruction::RotR),
            )),
            keyword("const.i").with(
                (positioned(primitive_type()), positioned(literal_integer()))
                    .map(|(ty, value)| ast::Instruction::ConstI(ty, value)),
            ),
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
        directive("entry", global_symbol()).map(ast::TopLevelDeclaration::Entry),
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
                        ast::FormatDeclaration::Major(registir::format::numeric::UInteger(0))
                    ),
                    ast::Positioned::new(
                        0,
                        20,
                        ast::FormatDeclaration::Minor(registir::format::numeric::UInteger(1))
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
