use crate::{
    ast,
    lexer::{self, Token},
};
use chumsky::{self, Parser};

pub type Error = chumsky::error::Simple<Token>;

pub type Tree = Vec<ast::Positioned<ast::TopLevelDeclaration>>;

fn parser() -> impl Parser<Token, Tree, Error = Error> {
    use chumsky::{
        primitive::{choice, empty, end, filter, filter_map, just},
        recovery,
    };

    fn directive<O, P: Parser<Token, O, Error = Error>>(
        name: &'static str,
        contents: P,
    ) -> impl Parser<Token, Option<O>, Error = Error> {
        filter(move |token| match token {
            Token::Directive(directive_name) => directive_name == name,
            _ => false,
        })
        .ignore_then(contents.map(Some))
        //.recover_with(recovery::skip_until([Token::Semicolon], |_| None))
        .then_ignore(just(Token::Semicolon))
    }

    fn between_brackets_or_else<O, P: Parser<Token, O, Error = Error>, F: Fn() -> O>(
        inner: P,
        err: F,
    ) -> impl Parser<Token, O, Error = Error> {
        inner
            .delimited_by(Token::OpenBracket, Token::CloseBracket)
            .recover_with(recovery::nested_delimiters(
                Token::OpenBracket,
                Token::CloseBracket,
                [],
                move |_| err(),
            ))
    }

    fn between_parenthesis_or_else<O, P: Parser<Token, O, Error = Error>, F: Fn() -> O>(
        inner: P,
        err: F,
    ) -> impl Parser<Token, O, Error = Error> {
        inner
            .delimited_by(Token::OpenParenthesis, Token::CloseParenthesis)
            .recover_with(recovery::nested_delimiters(
                Token::OpenParenthesis,
                Token::CloseParenthesis,
                [],
                move |_| err(),
            ))
    }

    fn directive_between_brackets<A, B, P, Q, F>(
        name: &'static str,
        outer: P,
        inner: Q,
        err: F,
    ) -> impl Parser<Token, Option<(A, B)>, Error = Error>
    where
        P: Parser<Token, A, Error = Error>,
        Q: Parser<Token, B, Error = Error>,
        F: Fn() -> B,
    {
        directive(name, outer.then(between_brackets_or_else(inner, err)))
    }

    fn with_position<O, P: Parser<Token, O, Error = Error>>(
        parser: P,
    ) -> impl Parser<Token, (O, ast::Position), Error = Error> {
        parser.map_with_span(|value, position| (value, position))
    }

    fn filter_parsed_declarations<D>(declarations: Vec<Option<D>>) -> Vec<D> {
        declarations
            .into_iter()
            .filter_map(|declaration| declaration)
            .collect()
    }

    fn directive_with_declarations<A, N, D, P, Q, F>(
        name: &'static str,
        attributes: P,
        declaration: Q,
        declarer: F,
    ) -> impl Parser<Token, Option<D>, Error = Error>
    where
        P: Parser<Token, A, Error = Error>,
        Q: Parser<Token, Option<N>, Error = Error>,
        F: Fn(A, Vec<N>) -> D,
    {
        directive_between_brackets(name, attributes, declaration.repeated(), Vec::new).map(
            move |content| {
                content.map(|(attributes, nodes)| {
                    declarer(attributes, filter_parsed_declarations(nodes))
                })
            },
        )
    }

    let global_symbol = {
        filter_map(|position, token| match token {
            Token::GlobalIdentifier(symbol) => Ok(ast::GlobalSymbol((symbol, position))),
            _ => Err(Error::custom(position, "expected global symbol")),
        })
    };

    let local_symbol = filter_map(|position, token| match token {
        Token::LocalIdentifier(symbol) => Ok(ast::LocalSymbol((symbol, position))),
        _ => Err(Error::custom(position, "expected local symbol")),
    });

    let register_symbol = filter_map(|position, token| match token {
        Token::RegisterIdentifier(symbol) => Ok(ast::RegisterSymbol((symbol, position))),
        _ => Err(Error::custom(position, "expected register symbol")),
    });

    fn with_position_optional<O, P: Parser<Token, Option<O>, Error = Error>>(
        parser: P,
    ) -> impl Parser<Token, Option<ast::Positioned<O>>, Error = Error> {
        with_position(parser).map(|(result, position)| result.map(|value| (value, position)))
    }

    fn simple_declaration<N, D, P, F>(
        name: &'static str,
        declaration: P,
        declarer: F,
    ) -> impl Parser<Token, Option<D>, Error = Error>
    where
        P: Parser<Token, Option<N>, Error = Error>,
        F: Fn(Vec<ast::Positioned<N>>) -> D,
    {
        directive_with_declarations(
            name,
            empty(),
            with_position_optional(declaration),
            move |(), nodes| declarer(nodes),
        )
    }

    fn symbolic_declaration<S, A, N, D, P, Q, R, F>(
        name: &'static str,
        symbol: P,
        attributes: Q,
        declaration: R,
        declarer: F,
    ) -> impl Parser<Token, Option<D>, Error = Error>
    where
        P: Parser<Token, S, Error = Error>,
        Q: Parser<Token, A, Error = Error>,
        R: Parser<Token, Option<N>, Error = Error>,
        F: Fn(S, A, Vec<N>) -> D,
    {
        directive_with_declarations(
            name,
            symbol.then(attributes),
            declaration,
            move |(s, a), nodes| declarer(s, a, nodes),
        )
    }

    let any_keyword = filter_map(|position, token| match token {
        Token::Keyword(word) => Ok(word),
        _ => Err(Error::custom(position, "expected keyword")),
    });

    let keyword = |word: &'static str| {
        any_keyword.try_map(move |actual, position| {
            if actual == word {
                Ok(())
            } else {
                Err(Error::custom(
                    position,
                    format!("expected {} but got {}", word, actual),
                ))
            }
        })
    };

    let integer_literal = filter_map(|position, token| match token {
        Token::LiteralInteger(value) => Ok(value),
        _ => Err(Error::custom(position, "expected integer literal")),
    });

    let string_literal = filter_map(|position, token| match token {
        Token::LiteralString(value) => Ok(value),
        _ => Err(Error::custom(position, "expected string literal")),
    });

    let identifier_literal = || {
        with_position(string_literal.try_map(|literal, position| {
            ast::Identifier::try_from(String::from(literal))
                .map_err(|_| Error::custom(position, "Expected non-empty string literal"))
        }))
    };

    let version_number = {
        integer_literal.try_map(|value, position| {
            u32::try_from(value).map_err(|_| Error::custom(position, "invalid version number"))
        })
    };

    macro_rules! name_directive {
        ($mapper: expr) => {
            directive("name", identifier_literal().map($mapper))
        };
    }

    let format_declaration = choice((
        directive("major", version_number.map(ast::FormatDeclaration::Major)),
        directive("minor", version_number.map(ast::FormatDeclaration::Minor)),
    ));

    let module_declaration = choice((
        name_directive!(ast::ModuleDeclaration::Name),
        directive(
            "version",
            version_number
                .repeated()
                .map(ast::ModuleDeclaration::Version),
        ),
    ));

    let primitive_type = choice((keyword("s32").to(ast::PrimitiveType::S32),));

    let many_registers = || register_symbol.separated_by(just(Token::Comma));

    let code_declaration = {
        // NOTE: Waiting for next stable release of chumsky for the then_with function
        /* let instruction_parsers = {
            let mut lookup = std::collections::HashMap::<&'static str, chumsky::BoxedParser<Token, ast::Instruction, Error>>::new();

            lookup.insert("nop", empty().to(ast::Instruction::Nop).boxed());

            lookup
        }; */

        let code_statement = {
            //any_keyword
            let full_instruction = with_position(choice((
                keyword("nop").to(ast::Instruction::Nop),
                keyword("ret")
                    .ignore_then(many_registers())
                    .map(ast::Instruction::Ret),
                keyword("const.i")
                    .ignore_then(with_position(primitive_type).then(with_position(integer_literal)))
                    .map(|(integer_type, value)| ast::Instruction::ConstI(integer_type, value)),
            )));

            let result_registers = many_registers()
                .at_least(1)
                .then_ignore(just(Token::Equals))
                .or_not()
                .map(|results| results.unwrap_or_else(Vec::new));

            with_position(result_registers)
                .then(full_instruction)
                .map(|(results, instruction)| {
                    Some(ast::Statement {
                        results,
                        instruction,
                    })
                })
                .then_ignore(just(Token::Semicolon))
        };

        choice((
            directive("entry", local_symbol.map(ast::CodeDeclaration::Entry)),
            symbolic_declaration(
                "block",
                local_symbol,
                between_parenthesis_or_else(many_registers(), Vec::new),
                code_statement,
                |name, arguments, instructions| ast::CodeDeclaration::Block {
                    name,
                    arguments,
                    instructions,
                },
            ),
        ))
    };

    let function_attributes = {
        let function_types = || {
            between_parenthesis_or_else(
                with_position(primitive_type.map(ast::Type::Primitive))
                    .separated_by(just(Token::Comma)),
                Vec::new,
            )
        };
        function_types().then(keyword("returns").ignore_then(
            function_types().then(keyword("export").ignore_then(identifier_literal()).or_not()),
        ))
    };

    let function_declaration = {
        let body_declaration = choice((
            keyword("defined")
                .ignore_then(global_symbol)
                .map(ast::FunctionBodyDeclaration::Defined),
            keyword("external").ignore_then(chumsky::primitive::todo()),
        ));

        choice((
            name_directive!(ast::FunctionDeclaration::Name),
            directive("body", body_declaration.map(ast::FunctionDeclaration::Body)),
        ))
    };

    let top_level_declaration = choice((
        simple_declaration(
            "format",
            format_declaration,
            ast::TopLevelDeclaration::Format,
        ),
        simple_declaration(
            "module",
            module_declaration,
            ast::TopLevelDeclaration::Module,
        ),
        directive("entry", global_symbol.map(ast::TopLevelDeclaration::Entry)),
        symbolic_declaration(
            "code",
            global_symbol,
            empty(),
            with_position_optional(code_declaration),
            |symbol, (), declarations| ast::TopLevelDeclaration::Code {
                symbol,
                declarations,
            },
        ),
        symbolic_declaration(
            "function",
            global_symbol,
            function_attributes,
            with_position_optional(function_declaration),
            |symbol, (parameter_types, (return_types, exported)), declarations| {
                ast::TopLevelDeclaration::Function {
                    symbol,
                    exported,
                    parameter_types,
                    return_types,
                    declarations,
                }
            },
        ),
    ));

    with_position_optional(top_level_declaration)
        .repeated()
        .map(filter_parsed_declarations)
        .then_ignore(end())
}

pub fn tree_from_str(input: &str) -> (Tree, Vec<lexer::Error>, Vec<Error>) {
    let (tokens, lexer_errors) = lexer::tokens_from_str(input);
    match tokens.last() {
        Some((_, last)) => {
            let (declarations, parser_errors) = parser()
                .parse_recovery(chumsky::Stream::from_iter(last.clone(), tokens.into_iter()));
            (
                declarations.unwrap_or_else(Vec::new),
                lexer_errors,
                parser_errors,
            )
        }
        None => (Vec::new(), Vec::new(), Vec::new()),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, Position, TopLevelDeclaration},
        parser,
    };

    macro_rules! assert_success {
        ($input: expr, $output: expr) => {
            assert_eq!(
                parser::tree_from_str($input),
                ($output, Vec::new(), Vec::new())
            )
        };
    }

    #[test]
    fn format_declaration_test() {
        assert_success!(
            ".format { .major 0; .minor 0xA; };",
            vec![(
                TopLevelDeclaration::Format(vec![
                    (
                        ast::FormatDeclaration::Major(0),
                        Position { start: 10, end: 19 }
                    ),
                    (
                        ast::FormatDeclaration::Minor(10),
                        Position { start: 20, end: 31 }
                    )
                ]),
                Position { start: 0, end: 34 }
            )]
        )
    }

    #[test]
    fn module_declaration_test() {
        assert_success!(
            ".module {\n    .name \"Hey\"; .version 1 0 0;\n};",
            vec![(
                TopLevelDeclaration::Module(vec![
                    (
                        ast::ModuleDeclaration::Name((
                            ast::Identifier::try_from("Hey").unwrap(),
                            Position { start: 20, end: 25 }
                        )),
                        Position { start: 14, end: 26 }
                    ),
                    (
                        ast::ModuleDeclaration::Version(vec![1, 0, 0]),
                        Position { start: 27, end: 42 }
                    )
                ]),
                Position { start: 0, end: 45 }
            )]
        )
    }

    #[test]
    fn entry_point_declaration_test() {
        assert_success!(
            "\n.entry @my_main_function;\n\n",
            vec![(
                TopLevelDeclaration::Entry(ast::GlobalSymbol((
                    ast::Identifier::try_from("my_main_function").unwrap(),
                    Position { start: 8, end: 25 }
                ))),
                Position { start: 1, end: 26 }
            )]
        )
    }

    #[test]
    fn basic_code_declaration_test() {
        assert_success!(
            ".code @code_test {\n  .entry $ENTRY;\n  .block $ENTRY () {\n    ret %non_existant;\n  };\n};\n",
            vec![(
                TopLevelDeclaration::Code {
                    symbol: ast::GlobalSymbol((
                        ast::Identifier::try_from("code_test").unwrap(),
                        Position { start: 6, end: 16 }
                    )),
                    declarations: vec![
                        (
                            ast::CodeDeclaration::Entry(ast::LocalSymbol((
                                ast::Identifier::try_from("ENTRY").unwrap(),
                                Position { start: 28, end: 34 }
                            ))),
                            Position { start: 21, end: 35 }
                        ),
                        (
                            ast::CodeDeclaration::Block {
                                name: ast::LocalSymbol((
                                    ast::Identifier::try_from("ENTRY").unwrap(),
                                    Position { start: 45, end: 51 }
                                )),
                                arguments: Vec::new(),
                                instructions: vec![ast::Statement {
                                    results: (Vec::new(), Position { start: 61, end: 62 }),
                                    instruction: (
                                        ast::Instruction::Ret(vec![
                                            ast::RegisterSymbol((
                                                ast::Identifier::try_from("non_existant").unwrap(),
                                                Position { start: 65, end: 78 }
                                            ))
                                        ]),
                                        Position { start: 61, end: 78 }
                                    )
                                }]
                            },
                            Position { start: 38, end: 84 }
                        )
                    ]
                },
                Position { start: 0, end: 87 }
            )]
        )
    }
}
