//! Provides functions for parsing SAILAR assembly.

use crate::ast;
use crate::lexer::{self, Token};
use std::iter::Iterator;
use std::ops::Range;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown token")]
    UnknownToken,
    #[error("expected end of line or file")]
    ExpectedNewLineOrEndOfFile,
    #[error("unknown directive \".{0}\"")]
    UnknownDirective(Box<str>),
    #[error("{0} is not a valid format version kind")]
    InvalidFormatVersionKind(Box<str>),
    #[error("expected format version kind")]
    ExpectedFormatVersionKind,
    #[error("expected integer format version")]
    ExpectedFormatVersion,
    #[error("invalid format version: {0}")]
    InvalidFormatVersion(std::num::ParseIntError),
    #[error("expected metadata field name")]
    ExpectedMetadataFieldName,
    #[error("{0} is not a known metadata field name")]
    UnknownMetadataFieldName(Box<str>),
    #[error("expected metadata module name")]
    ExpectedMetadataModuleName,
    #[error("expected literal contents of identifier")]
    ExpectedIdentifierLiteral,
    #[error("\"\\{0}\" is not a valid escape sequence")]
    InvalidEscapeSequence(Box<str>),
    #[error(transparent)]
    InvalidIdentifierLiteral(#[from] sailar::identifier::InvalidError),
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Error {
    kind: Box<ErrorKind>,
    location: ast::LocationRange,
}

impl Error {
    pub fn new<K: Into<ErrorKind>, L: Into<ast::LocationRange>>(kind: K, location: L) -> Self {
        Self {
            kind: Box::new(kind.into()),
            location: location.into(),
        }
    }

    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    #[inline]
    pub fn location(&self) -> &ast::LocationRange {
        &self.location
    }
}

struct Input<'tree, 'source, S: Iterator> {
    source: std::iter::Peekable<S>,
    locations: &'tree lexer::OffsetMap<'source>,
}

type SliceInput<'tree, 'source> = Input<'tree, 'source, std::slice::Iter<'tree, (Token<'source>, Range<usize>)>>;

type LocatedToken<'tree, 'source> = (&'tree (Token<'source>, Range<usize>), ast::LocationRange);

impl<'tree, 'source, S: Iterator<Item = &'tree (Token<'source>, Range<usize>)>> Input<'tree, 'source, S> {
    fn new<I: std::iter::IntoIterator<IntoIter = S>>(source: I, locations: &'tree lexer::OffsetMap<'source>) -> Self {
        Self {
            source: source.into_iter().peekable(),
            locations,
        }
    }

    fn token_from_offsets(
        token: Option<&S::Item>,
        locations: &'tree lexer::OffsetMap<'source>,
    ) -> Option<LocatedToken<'tree, 'source>> {
        token.map(|lexer_token| {
            let (_, offsets) = lexer_token;
            (
                *lexer_token,
                ast::LocationRange::new(
                    locations.get_location(offsets.start).unwrap(),
                    locations.get_location(offsets.end).unwrap(),
                ),
            )
        })
    }

    fn next_token(&mut self) -> Option<LocatedToken<'tree, 'source>> {
        Self::token_from_offsets(self.source.next().as_ref(), self.locations)
    }

    fn next_token_if<F: FnOnce(&'tree Token<'source>) -> bool>(&mut self, condition: F) -> Option<LocatedToken<'tree, 'source>> {
        Self::token_from_offsets(self.source.next_if(|(token, _)| condition(token)).as_ref(), self.locations)
    }

    fn peek_next_token(&mut self) -> Option<LocatedToken<'tree, 'source>> {
        Self::token_from_offsets(self.source.peek(), self.locations)
    }
}

#[derive(Debug, Default)]
pub struct Output<'source> {
    tree: Vec<ast::Located<ast::Directive<'source>>>,
    errors: Vec<Error>,
}

impl<'source> Output<'source> {
    #[inline]
    pub fn tree(&self) -> &[ast::Located<ast::Directive<'source>>] {
        &self.tree
    }

    #[inline]
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

enum Parser<'tree, 'source> {
    Pointer(fn(&mut State<'tree, 'source>) -> Option<Self>),
    Closure(Box<dyn FnMut(&mut State<'tree, 'source>) -> Option<Self>>),
}

impl<'tree, 'source> Parser<'tree, 'source> {
    fn parse(&mut self, state: &mut State<'tree, 'source>) -> Option<Self> {
        match self {
            Self::Pointer(pointer) => (pointer)(state),
            Self::Closure(boxed) => (boxed)(state),
        }
    }
}

struct State<'tree, 'source> {
    input: SliceInput<'tree, 'source>,
    character_buffer: String,
    output: Output<'source>,
}

impl<'tree, 'source> State<'tree, 'source> {
    fn push_error<E: Into<ErrorKind>, L: Into<ast::LocationRange>>(&mut self, error: E, location: L) {
        self.output.errors.push(Error::new(error, location));
    }
}

macro_rules! fail_continue {
    ($state: expr, $error: expr, $location: expr, $next_parser: expr) => {{
        $state.push_error($error, $location);
        return $next_parser.into();
    }};
}

macro_rules! fail_skip_line {
    ($state: expr, $error: expr, $location: expr, $next_parser: expr) => {{
        $state.push_error($error, $location);

        loop {
            match $state.input.next_token() {
                Some(((Token::Newline, _), _)) | None => break,
                Some(_) => (),
            }
        }

        return $next_parser.into();
    }};
}

macro_rules! fail_match_exhausted {
    ($state: expr, $error: expr, $token: expr, $next_parser: expr) => {
        fail_continue!(
            $state,
            $error,
            if let Some((_, location)) = $token {
                location
            } else {
                $state.input.locations.get_last().unwrap().into()
            },
            $next_parser
        )
    };
}

macro_rules! expect_new_line_or_end {
    ($state: expr, $next_parser: expr) => {
        match $state.input.peek_next_token() {
            Some(((Token::Newline, _), _)) | None => (),
            Some((_, location)) => {
                let start_location = location.start();
                let mut end_location = location.end().clone();

                while let Some((_, location)) = $state.input.next_token_if(|next| !matches!(next, Token::Newline)) {
                    end_location = location.end().clone();
                }

                fail_continue!(
                    $state,
                    ErrorKind::ExpectedNewLineOrEndOfFile,
                    ast::LocationRange::new(start_location.clone(), end_location),
                    $next_parser
                );
            }
        }
    };
}

/// Parsers a literal string that also qualifies as a valid SAILAR identifier.
fn parse_literal_identifier<'tree, 'source>(
    state: &mut State<'tree, 'source>,
    success_parser: impl FnOnce(ast::Located<ast::Identifier<'source>>) -> Parser<'tree, 'source>,
    failed_parser: Parser<'tree, 'source>,
) -> Option<Parser<'tree, 'source>> {
    match state.input.next_token() {
        Some(((Token::LiteralString(contents), literal_offset), location)) => {
            match ast::LiteralString::with_escape_sequences(contents, &mut state.character_buffer) {
                Ok(literal) => match literal.try_into_identifier() {
                    Ok(identifier) => {
                        return Some(success_parser(ast::Located::with_range(identifier, location)));
                    }
                    Err(e) => fail_skip_line!(state, e, location, failed_parser),
                },
                Err(e) => {
                    let escape_start_location = literal_offset.start + 1 + e.byte_offset();
                    let escape_end_location = escape_start_location + e.sequence().len() + 1;
                    fail_skip_line!(
                        state,
                        ErrorKind::InvalidEscapeSequence(e.take_sequence()),
                        ast::LocationRange::new(
                            state.input.locations.get_location(escape_start_location).unwrap(),
                            state.input.locations.get_location(escape_end_location).unwrap()
                        ),
                        failed_parser
                    );
                }
            }
        }
        bad => fail_match_exhausted!(state, ErrorKind::ExpectedIdentifierLiteral, bad, failed_parser),
    }
}

fn parse_format_directive<'tree, 'source>(state: &mut State<'tree, 'source>) -> Option<Parser<'tree, 'source>> {
    let format_kind = match state.input.next_token() {
        Some(((Token::Word("major"), _), _)) => ast::FormatVersionKind::Major,
        Some(((Token::Word("minor"), _), _)) => ast::FormatVersionKind::Minor,
        Some(((Token::Word(bad), _), location)) => fail_skip_line!(
            state,
            ErrorKind::InvalidFormatVersionKind(Box::from(*bad)),
            location,
            Parser::Pointer(parse_directive)
        ),
        bad => fail_match_exhausted!(
            state,
            ErrorKind::ExpectedFormatVersionKind,
            bad,
            Parser::Pointer(parse_directive)
        ),
    };

    let end_location;
    let format_version = match state.input.next_token() {
        Some(((Token::LiteralInteger(digits), _), location)) => {
            end_location = location.end().clone();
            match u8::try_from(digits) {
                Ok(version) => version,
                Err(e) => fail_skip_line!(
                    state,
                    ErrorKind::InvalidFormatVersion(e),
                    location,
                    Parser::Pointer(parse_directive)
                ),
            }
        }
        bad => fail_match_exhausted!(state, ErrorKind::ExpectedFormatVersion, bad, Parser::Pointer(parse_directive)),
    };

    expect_new_line_or_end!(state, Parser::Pointer(parse_directive));

    state.output.tree.push(ast::Located::new(
        ast::Directive::Format(format_kind, format_version),
        todo!("location"), //state.current_location,
        end_location,
    ));

    return Some(Parser::Pointer(parse_directive));
}

fn parse_directive<'tree, 'source>(state: &mut State<'tree, 'source>) -> Option<Parser<'tree, 'source>> {
    state.input.next_token().and_then(|((token, _), location)| {
        match token {
            Token::Newline => (),
            Token::ArrayDirective => state
                .output
                .tree
                .push(ast::Located::with_range(ast::Directive::Array, location)),
            Token::FormatDirective => return Some(Parser::Pointer(parse_format_directive)),
            Token::UnknownDirective(directive) => fail_skip_line!(
                state,
                ErrorKind::UnknownDirective(Box::from(*directive)),
                location,
                Parser::Pointer(parse_directive)
            ),
            Token::Unknown | _ => fail_continue!(state, ErrorKind::UnknownToken, location, Parser::Pointer(parse_directive)),
        }

        Some(Parser::Pointer(parse_directive))
    })
}

/// Transfers a sequence of tokens into an abstract syntax tree.
pub fn parse<'source>(input: &lexer::Output<'source>) -> Output<'source> {
    let mut current_parser = Some(Parser::Pointer(parse_directive));
    let mut state = State {
        input: SliceInput::new(input.tokens(), input.locations()),
        character_buffer: String::default(),
        output: Output::default(),
    };

    while let Some(mut parser) = current_parser {
        current_parser = parser.parse(&mut state);
    }

    state.output
    
    //     match token {
    //         Token::MetadataDirective => match input.next_token() {
    //             Some(((Token::Word("id"), _), _)) => {
    //                 // TODO: Use helper function for parsing identifier string.
    //                 let name = match input.next_token() {

    //                     bad => match_exhausted!(errors, ErrorKind::ExpectedMetadataModuleName, bad, input),
    //                 };

    //                 // TODO: When parsing version when match_exhausted just do UnknownToken or ExpectedEofOrNewLine
    //                 todo!("module id");
    //             }
    //             Some(((Token::Word(unknown), _), location)) => fail_skip_line!(
    //                 errors,
    //                 ErrorKind::UnknownMetadataFieldName(Box::from(*unknown)),
    //                 location,
    //                 input
    //             ),
    //             bad => match_exhausted!(errors, ErrorKind::ExpectedMetadataFieldName, bad, input),
    //         },
    //         Token::IdentifierDirective => {
    //             let symbol = None;

    //             let literal_start_location;
    //             let literal = match input.next_token() {
    //                 Some(((Token::LiteralString(contents), literal_offset), location)) => {
    //                     // TODO: Make helper function for parsing identifier string.
    //                     match ast::LiteralString::with_escape_sequences(contents, &mut character_buffer) {
    //                         Ok(literal) => {
    //                             literal_start_location = location.start().clone();
    //                             end_location = location.end().clone();
    //                             literal
    //                         }
    //                         Err(e) => {
    //                             let escape_start_location = literal_offset.start + 1 + e.byte_offset();
    //                             let escape_end_location = escape_start_location + e.sequence().len() + 1;
    //                             fail_skip_line!(
    //                                 errors,
    //                                 ErrorKind::InvalidEscapeSequence(e.take_sequence()),
    //                                 ast::LocationRange::new(
    //                                     input.locations.get_location(escape_start_location).unwrap(),
    //                                     input.locations.get_location(escape_end_location).unwrap()
    //                                 ),
    //                                 input
    //                             );
    //                         }
    //                     }
    //                 }
    //                 bad => match_exhausted!(errors, ErrorKind::ExpectedIdentifierLiteral, bad, input),
    //             };

    //             expect_new_line_or_end!(errors, input);

    //             let identifier = match literal.try_into_identifier() {
    //                 Ok(identifier) => identifier,
    //                 Err(e) => fail_skip_line!(
    //                     errors,
    //                     e,
    //                     ast::LocationRange::new(literal_start_location, end_location),
    //                     input
    //                 ),
    //             };

    //             tree.push(ast::Located::new(
    //                 ast::Directive::Identifier(symbol, identifier),
    //                 start_location,
    //                 end_location,
    //             ));
    //         }
    //     }
    // }
}
