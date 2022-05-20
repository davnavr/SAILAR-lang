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

    fn skip_current_line(&mut self) {
        loop {
            match self.next_token() {
                Some(((Token::Newline, _), _)) | None => break,
                Some(_) => (),
            }
        }
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

struct State<'tree, 'source> {
    input: SliceInput<'tree, 'source>,
    character_buffer: String,
    output: Output<'source>,
}

impl<'tree, 'source> State<'tree, 'source> {
    fn locations(&self) -> &'tree lexer::OffsetMap<'source> {
        self.input.locations
    }

    fn push_error<E: Into<ErrorKind>, L: Into<ast::LocationRange>>(&mut self, error: E, location: L) {
        self.output.errors.push(Error::new(error, location));
    }

    fn expect_newline_or_end(&mut self) {
        match self.input.peek_next_token() {
            Some(((Token::Newline, _), _)) | None => (),
            Some((_, location)) => {
                let start_location = location.start();
                let mut end_location = location.end().clone();

                while let Some((_, location)) = self.input.next_token_if(|next| !matches!(next, Token::Newline)) {
                    end_location = location.end().clone();
                }

                self.push_error(
                    ErrorKind::ExpectedNewLineOrEndOfFile,
                    ast::LocationRange::new(start_location.clone(), end_location),
                );
            }
        }
    }
}

fn token_location_or_last(token: Option<&LocatedToken>, locations: &lexer::OffsetMap) -> ast::LocationRange {
    token
        .map(|(_, location)| location.clone())
        .unwrap_or_else(|| locations.get_last().unwrap().into())
}

/// Parses a token containing a valid stirng literal.
fn parse_literal_string<'t, 's>(
    state: &mut State<'t, 's>,
    mut success: impl FnMut(&mut State<'t, 's>, ast::Located<ast::LiteralString<'s>>),
    mut exhausted: impl FnMut(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
) {
    match state.input.next_token() {
        Some(((Token::LiteralString(contents), literal_offset), location)) => {
            match ast::LiteralString::with_escape_sequences(contents, &mut state.character_buffer) {
                Ok(s) => success(state, ast::Located::with_range(s, location)),
                Err(error) => {
                    let escape_start_location = literal_offset.start + 1 + error.byte_offset();
                    let escape_end_location = escape_start_location + error.sequence().len() + 1;
                    state.push_error(
                        ErrorKind::InvalidEscapeSequence(error.take_sequence()),
                        ast::LocationRange::new(
                            state.input.locations.get_location(escape_start_location).unwrap(),
                            state.input.locations.get_location(escape_end_location).unwrap(),
                        ),
                    );
                }
            }
        }
        bad => exhausted(state, bad),
    }
}

/// Parsers a literal string containing a valid SAILAR identifier.
fn parse_literal_identifier<'t, 's>(
    state: &mut State<'t, 's>,
    mut success: impl FnMut(&mut State<'t, 's>, ast::Located<ast::Identifier<'s>>),
    exhausted: impl FnMut(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
) {
    parse_literal_string(
        state,
        |state, token| {
            let (literal, location) = token.into();
            match literal.try_into_identifier() {
                Ok(identifier) => success(state, ast::Located::with_range(identifier, location)),
                Err(error) => state.push_error(error, location),
            }
        },
        exhausted,
    );
}

/// Transfers a sequence of tokens into an abstract syntax tree.
pub fn parse<'source>(input: &lexer::Output<'source>) -> Output<'source> {
    let mut state = State {
        input: SliceInput::new(input.tokens(), input.locations()),
        character_buffer: String::default(),
        output: Output::default(),
    };

    while let Some(((token, _), location)) = state.input.next_token() {
        match token {
            Token::Newline => (),
            Token::ArrayDirective => state
                .output
                .tree
                .push(ast::Located::with_range(ast::Directive::Array, location)),
            Token::FormatDirective => {
                let format_kind = match state.input.next_token() {
                    Some(((Token::Word("major"), _), _)) => ast::FormatVersionKind::Major,
                    Some(((Token::Word("minor"), _), _)) => ast::FormatVersionKind::Minor,
                    Some(((Token::Word(bad), _), location)) => {
                        state.push_error(ErrorKind::InvalidFormatVersionKind(Box::from(*bad)), location);
                        state.input.skip_current_line();
                        continue;
                    }
                    bad => {
                        state.push_error(
                            ErrorKind::ExpectedFormatVersionKind,
                            token_location_or_last(bad.as_ref(), input.locations()),
                        );
                        state.input.skip_current_line();
                        continue;
                    }
                };

                let end_location;
                let format_version = match state.input.next_token() {
                    Some(((Token::LiteralInteger(digits), _), location)) => {
                        end_location = location.end().clone();
                        match u8::try_from(digits) {
                            Ok(version) => version,
                            Err(e) => {
                                state.push_error(ErrorKind::InvalidFormatVersion(e), location);
                                state.input.skip_current_line();
                                continue;
                            }
                        }
                    }
                    bad => {
                        state.push_error(
                            ErrorKind::ExpectedFormatVersion,
                            token_location_or_last(bad.as_ref(), input.locations()),
                        );
                        state.input.skip_current_line();
                        continue;
                    }
                };

                state.expect_newline_or_end();

                state.output.tree.push(ast::Located::new(
                    ast::Directive::Format(format_kind, format_version),
                    location.start().clone(),
                    end_location,
                ));
            }
            Token::IdentifierDirective => {
                let symbol = None;

                parse_literal_identifier(
                    &mut state,
                    move |state, identifier| {
                        let end_location = identifier.location().end().clone();
                        state.output.tree.push(ast::Located::new(
                            ast::Directive::Identifier(symbol.clone(), identifier),
                            location.start().clone(),
                            end_location,
                        ));
                    },
                    |state, token| {
                        state.push_error(
                            ErrorKind::ExpectedIdentifierLiteral,
                            token_location_or_last(token.as_ref(), state.locations()),
                        )
                    },
                )
            }
            Token::UnknownDirective(directive) => state.push_error(ErrorKind::UnknownDirective(Box::from(*directive)), location),
            Token::Unknown | _ => state.push_error(ErrorKind::UnknownToken, location),
        }
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
    //     }
    // }
}
