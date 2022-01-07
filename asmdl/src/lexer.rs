use crate::ast;
use chumsky::{self, Parser};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    Comma,
    Equals,
    Semicolon,
    Directive(String),
    GlobalIdentifier(ast::Identifier),
    LocalIdentifier(ast::Identifier),
    RegisterIdentifier(ast::Identifier),
    LiteralInteger(i128),
    /// A string enclosed in quotation marks.
    LiteralString(ast::LiteralString),
    /// An instruction or other keyword.
    Keyword(String),
}

pub type Error = chumsky::error::Simple<char>;

pub fn tokenizer() -> impl Parser<char, Vec<ast::Positioned<Token>>, Error = Error> {
    use chumsky::{
        primitive::{end, filter, just, none_of, take_until},
        recovery,
        text::{self, newline, TextParser as _},
    };

    let characters = {
        fn char_token(c: char, token: Token) -> impl Parser<char, Token, Error = Error> {
            just(c).to(token)
        }

        char_token('{', Token::OpenBracket)
            .or(char_token('}', Token::CloseBracket))
            .or(char_token('(', Token::OpenParenthesis))
            .or(char_token(')', Token::CloseParenthesis))
            .or(char_token(',', Token::Comma))
            .or(char_token('=', Token::Equals))
            .or(char_token(';', Token::Semicolon))
    };

    let directive = just('.').ignore_then(text::ident()).map(Token::Directive);

    let identifiers = {
        fn prefix_ident<T: Fn(ast::Identifier) -> Token>(
            prefix: char,
            token: T,
        ) -> impl Parser<char, Token, Error = Error> {
            just(prefix)
                .ignore_then(
                    filter(|&c: &char| c.is_alphanumeric())
                        .repeated()
                        .at_least(1)
                        .collect::<String>(),
                )
                .map(move |identifier| token(ast::Identifier::try_from(identifier).unwrap()))
        }

        prefix_ident('@', Token::GlobalIdentifier)
            .or(prefix_ident('$', Token::LocalIdentifier))
            .or(prefix_ident('%', Token::RegisterIdentifier))
    };

    let comment = just("//")
        .then(take_until(newline()))
        .or(just("/*").then(take_until(just("*/").ignored())))
        .padded();

    let integer_literal = {
        fn integer_digits(radix: u32) -> impl Parser<char, i128, Error = Error> {
            filter(move |&c: &char| c.is_digit(radix))
                //.separated_by(just('_').repeated())
                .repeated()
                .at_least(1)
                //.flatten()
                .collect::<String>()
                .try_map(move |digits, location| {
                    i128::from_str_radix(&digits, radix)
                        .map_err(|_| Error::custom(location, "Invalid integer literal"))
                })
        }

        just("0x")
            .ignore_then(integer_digits(16))
            .or(just("0b").ignore_then(integer_digits(2)))
            .or(integer_digits(10))
            .map(Token::LiteralInteger)
    };

    let string_literal = {
        let escape_sequence =
        just('n').to('\n')
            .or(just('t').to('\t'))
            .or(just('r').to('\r'))
            .or(just('\\').to('\\'))
            .or(just('"').to('"'))
            .or(just('\'').to('\''))
            // .or(just('u').ignore_then(
            //     filter(|&c: &char| c.is_digit(16)).repeated().exactly(4)
            // ))
            ;

        let character = (just('\\').ignore_then(escape_sequence)).or(none_of("\"\n"));

        character
            .repeated()
            .map(|contents| Token::LiteralString(ast::LiteralString(contents)))
            .delimited_by('\"', '\"')
    };

    let keyword = filter(move |c: &char| c.is_ascii_alphanumeric() || *c == '.')
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Keyword);

    characters
        .or(directive)
        .or(identifiers)
        .or(integer_literal)
        .or(string_literal)
        .or(keyword)
        .recover_with(recovery::skip_then_retry_until([]))
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
        .then_ignore(end())
}

pub fn tokenize(input: &str) -> (Vec<ast::Positioned<Token>>, Vec<Error>) {
    let (tokens, errors) = tokenizer().parse_recovery(input);
    (tokens.unwrap_or_else(Vec::new), errors)
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{LiteralString, Position},
        lexer::{tokenize, Token},
    };

    macro_rules! assert_no_errors {
        ($input: expr, $output: expr) => {
            assert_eq!(tokenize($input), ($output, Vec::new()))
        };
    }

    #[test]
    fn basic_sequence_test() {
        assert_no_errors!(
            "{ret;42",
            vec![
                (Token::OpenBracket, Position { start: 0, end: 1 }),
                (
                    Token::Keyword(String::from("ret")),
                    Position { start: 1, end: 4 }
                ),
                (Token::Semicolon, Position { start: 4, end: 5 }),
                (Token::LiteralInteger(42), Position { start: 5, end: 7 }),
            ]
        )
    }

    #[test]
    fn format_directive_tokens_test() {
        assert_no_errors!(
            ".format {\n  .major 0;\n  .minor 0x10;\n}",
            vec![
                (
                    Token::Directive(String::from("format")),
                    Position { start: 0, end: 7 }
                ),
                (Token::OpenBracket, Position { start: 8, end: 9 }),
                (
                    Token::Directive(String::from("major")),
                    Position { start: 12, end: 18 }
                ),
                (Token::LiteralInteger(0), Position { start: 19, end: 20 }),
                (Token::Semicolon, Position { start: 20, end: 21 }),
                (
                    Token::Directive(String::from("minor")),
                    Position { start: 24, end: 30 }
                ),
                (Token::LiteralInteger(16), Position { start: 31, end: 35 }),
                (Token::Semicolon, Position { start: 35, end: 36 }),
                (Token::CloseBracket, Position { start: 37, end: 38 }),
            ]
        )
    }

    #[test]
    fn module_directive_tokens_test() {
        assert_no_errors!(
            ".module { .name \"Hello\" };",
            vec![
                (
                    Token::Directive(String::from("module")),
                    Position { start: 0, end: 7 }
                ),
                (Token::OpenBracket, Position { start: 8, end: 9 }),
                (
                    Token::Directive(String::from("name")),
                    Position { start: 10, end: 15 }
                ),
                (
                    Token::LiteralString(LiteralString::from("Hello")),
                    Position { start: 16, end: 23 }
                ),
                (Token::CloseBracket, Position { start: 24, end: 25 }),
                (Token::Semicolon, Position { start: 25, end: 26 }),
            ]
        );
    }
}
