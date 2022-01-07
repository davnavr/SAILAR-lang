use crate::{
    ast,
    lexer::{self, Token},
};
use chumsky::{self, Parser};

pub type Error = chumsky::error::Simple<Token>;

pub type Tree = Vec<ast::Positioned<ast::TopLevelDeclaration>>;

fn parser() -> impl Parser<Token, Tree, Error = Error> {
    use chumsky::{
        primitive::{choice, end, filter, filter_map, just},
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

    fn between_brackets<O, P: Parser<Token, O, Error = Error>>(
        inner: P,
    ) -> impl Parser<Token, Option<O>, Error = Error> {
        inner
            .delimited_by(Token::OpenBracket, Token::CloseBracket)
            .map(Some)
            .recover_with(recovery::nested_delimiters(
                Token::OpenBracket,
                Token::CloseBracket,
                [],
                |_| None,
            ))
    }

    fn directive_between_brackets<O, P: Parser<Token, O, Error = Error>>(
        name: &'static str,
        contents: P,
    ) -> impl Parser<Token, Option<O>, Error = Error> {
        directive(name, between_brackets(contents)).map(Option::flatten)
    }

    fn with_position<O, P: Parser<Token, O, Error = Error>>(
        parser: P,
    ) -> impl Parser<Token, (O, ast::Position), Error = Error> {
        parser.map_with_span(|value, position| (value, position))
    }

    fn filter_parsed_declarations<D>(
        declarations: Vec<ast::Positioned<Option<D>>>,
    ) -> Vec<ast::Positioned<D>> {
        declarations
            .into_iter()
            .filter_map(|(declaration, position)| declaration.map(|node| (node, position)))
            .collect()
    }

    fn directive_with_declarations<
        N,
        D,
        P: Parser<Token, Option<N>, Error = Error>,
        F: Fn(Vec<ast::Positioned<N>>) -> D,
    >(
        name: &'static str,
        declaration: P,
        declarer: F,
    ) -> impl Parser<Token, Option<D>, Error = Error> {
        directive_between_brackets(
            name,
            with_position(declaration)
                .repeated()
                .map(move |nodes| declarer(filter_parsed_declarations(nodes))),
        )
    }

    let integer_literal = filter_map(|position, token| match token {
        Token::LiteralInteger(value) => Ok(value),
        _ => Err(Error::custom(position, "expected integer literal")),
    });

    let string_literal = filter_map(|position, token| match token {
        Token::LiteralString(value) => Ok(value),
        _ => Err(Error::custom(position, "expected string literal")),
    });

    let identifier_literal = with_position(string_literal.try_map(|literal, position| {
        ast::Identifier::try_from(String::from(literal))
            .map_err(|_| Error::custom(position, "Expected non-empty string literal"))
    }));

    let format_declaration = choice((
        directive("major", integer_literal.map(ast::FormatDeclaration::Major)),
        directive("minor", integer_literal.map(ast::FormatDeclaration::Minor)),
    ));

    let module_declaration = choice((
        directive("name", identifier_literal.map(ast::ModuleDeclaration::Name)),
        directive(
            "version",
            integer_literal
                .try_map(|value, position| {
                    u32::try_from(value)
                        .map_err(|_| Error::custom(position, "invalid version number"))
                })
                .repeated()
                .map(ast::ModuleDeclaration::Version),
        ),
    ));

    let top_level_declaration = choice((
        directive_with_declarations(
            "format",
            format_declaration,
            ast::TopLevelDeclaration::Format,
        ),
        directive_with_declarations(
            "module",
            module_declaration,
            ast::TopLevelDeclaration::Module,
        ),
    ));

    with_position(top_level_declaration)
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
}
