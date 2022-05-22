//! Provides functions for parsing SAILAR assembly.

use crate::ast;
use crate::lexer::{self, Token};
use std::iter::Iterator;
use std::ops::Range;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("unknown token")]
    UnknownToken,
    #[error("\"\\{0}\" is not a valid escape sequence")]
    InvalidEscapeSequence(Box<str>),
    #[error("invalid integer literal: {0}")]
    InvalidIntegerLiteral(std::num::ParseIntError),
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
    #[error("expected another module version number")]
    ExpectedMetadataModuleVersionNumber,
    #[error("expected literal contents of identifier")]
    ExpectedIdentifierLiteral,
    #[error(transparent)]
    InvalidIdentifierLiteral(#[from] sailar::identifier::InvalidError),
    #[error("invalid byte literal: {0}")]
    InvalidByteLiteral(std::num::ParseIntError),
    #[error("expected kind of signature")]
    ExpectedSignatureKind,
    #[error("{0} is not a valid signature kind")]
    InvalidSignatureKind(Box<str>),
    #[error("not a valid type signature")]
    InvalidTypeSignature,
    #[error("{0} is not a valid primitive type")]
    UnknownPrimitiveType(Box<str>),
    #[error("not a valid pointee type")]
    InvalidPointeeType,
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

fn parse_literal_integer<'t, 's>(
    state: &mut State<'t, 's>,
    success: impl FnOnce(&mut State<'t, 's>, ast::Located<u32>),
    exhausted: impl FnOnce(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
) {
    match state.input.next_token() {
        Some(((Token::LiteralInteger(digits), _), location)) => match u32::try_from(digits) {
            Ok(value) => success(state, ast::Located::with_range(value, location)),
            Err(e) => state.push_error(ErrorKind::InvalidIntegerLiteral(e), location),
        },
        bad => exhausted(state, bad),
    }
}

/// Parses a token containing a valid stirng literal.
fn parse_literal_string<'t, 's>(
    state: &mut State<'t, 's>,
    success: impl FnOnce(&mut State<'t, 's>, ast::Located<ast::LiteralString<'s>>),
    exhausted: impl FnOnce(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
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

/// Parses a literal string containing a valid SAILAR identifier.
fn parse_literal_identifier<'t, 's>(
    state: &mut State<'t, 's>,
    success: impl FnOnce(&mut State<'t, 's>, ast::Located<ast::Identifier<'s>>),
    exhausted: impl FnOnce(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
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

fn parse_symbol<'t, 's>(state: &mut State<'t, 's>) -> Option<ast::Symbol<'s>> {
    state.input.peek_next_token().and_then(|token| match token {
        ((Token::Symbol(symbol), _), location) => {
            state.input.next_token();
            Some(ast::Located::with_range(*symbol, location))
        }
        _ => None,
    })
}

fn parse_reference<'t, 's>(
    state: &mut State<'t, 's>,
    success: impl FnOnce(&mut State<'t, 's>, ast::Reference<'s>),
    exhausted: impl FnOnce(&mut State<'t, 's>, Option<LocatedToken<'t, 's>>),
) {
    match state.input.next_token() {
        Some(((Token::Symbol(symbol), _), location)) => {
            success(state, ast::Reference::Symbol(ast::Located::with_range(*symbol, location)))
        }
        Some(((Token::LiteralInteger(digits), _), location)) => match u32::try_from(digits) {
            Ok(index) => success(state, ast::Reference::Index(ast::Located::with_range(index, location))),
            Err(e) => state.push_error(ErrorKind::InvalidIntegerLiteral(e), location),
        },
        bad => exhausted(state, bad),
    }
}

/// Transfers a sequence of tokens into an abstract syntax tree.
pub fn parse<'source>(input: &lexer::Output<'source>) -> Output<'source> {
    let mut state = State {
        input: SliceInput::new(input.tokens(), input.locations()),
        character_buffer: String::default(),
        output: Output::default(),
    };

    while let Some(((token, _), start_location)) = state.input.next_token() {
        match token {
            Token::Newline => (),
            Token::Directive("array") => state
                .output
                .tree
                .push(ast::Located::with_range(ast::Directive::Array, start_location)),
            Token::Directive("format") => {
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
                    start_location.start().clone(),
                    end_location,
                ));
            }
            Token::Directive("metadata") => match state.input.next_token() {
                Some(((Token::Word(kind), _), location)) => match *kind {
                    "id" => parse_literal_identifier(
                        &mut state,
                        move |state, identifier| {
                            let mut version = Vec::default();
                            let mut end_location = identifier.location().end().clone();

                            parse_literal_integer(
                                state,
                                |state, first_number| {
                                    end_location = first_number.location().end().clone();
                                    version.push(*first_number.item());
                                    let mut failed = false;
                                    while !failed && state.input.next_token_if(|t| matches!(t, Token::Period)).is_some() {
                                        parse_literal_integer(
                                            state,
                                            |_, number| {
                                                end_location = number.location().end().clone();
                                                version.push(*number.item());
                                            },
                                            |state, bad| {
                                                state.push_error(
                                                    ErrorKind::ExpectedMetadataModuleVersionNumber,
                                                    token_location_or_last(bad.as_ref(), state.locations()),
                                                );
                                                failed = true;
                                            },
                                        )
                                    }
                                },
                                |_, _| (),
                            );

                            state.expect_newline_or_end();
                            state.output.tree.push(ast::Located::new(
                                ast::Directive::Metadata(ast::Metadata::Identifier(identifier, version.into_boxed_slice())),
                                start_location.start().clone(),
                                end_location,
                            ));
                        },
                        |state, bad| {
                            state.push_error(
                                ErrorKind::ExpectedMetadataModuleName,
                                token_location_or_last(bad.as_ref(), state.locations()),
                            );
                            state.input.skip_current_line();
                        },
                    ),
                    unknown => {
                        state.push_error(ErrorKind::UnknownMetadataFieldName(Box::from(unknown)), location);
                        state.input.skip_current_line();
                        continue;
                    }
                },
                bad => {
                    state.push_error(
                        ErrorKind::ExpectedMetadataFieldName,
                        token_location_or_last(bad.as_ref(), input.locations()),
                    );
                    state.input.skip_current_line();
                    continue;
                }
            },
            Token::Directive("identifier") => {
                let symbol = parse_symbol(&mut state);

                parse_literal_identifier(
                    &mut state,
                    move |state, identifier| {
                        let end_location = identifier.location().end().clone();
                        state.output.tree.push(ast::Located::new(
                            ast::Directive::Identifier(symbol, identifier),
                            start_location.start().clone(),
                            end_location,
                        ));
                    },
                    |state, token| {
                        state.push_error(
                            ErrorKind::ExpectedIdentifierLiteral,
                            token_location_or_last(token.as_ref(), state.locations()),
                        );
                        state.input.skip_current_line();
                    },
                )
            }
            Token::Directive("data") => {
                let symbol = parse_symbol(&mut state);
                let mut data = Vec::default();
                let mut end_location = start_location.end().clone();
                let mut data_start_location = end_location.clone();
                let mut data_location_updated = false;

                while let Some(((Token::LiteralInteger(digits), _), location)) = state.input.next_token() {
                    if !data_location_updated {
                        data_start_location = location.start().clone();
                        data_location_updated = true;
                    }

                    end_location = location.end().clone();

                    match u8::try_from(digits) {
                        Ok(byte) => data.push(byte),
                        Err(e) => state.push_error(ErrorKind::InvalidByteLiteral(e), location),
                    }
                }

                state.expect_newline_or_end();

                state.output.tree.push(ast::Located::new(
                    ast::Directive::Data(
                        symbol,
                        ast::Located::new(data.into_boxed_slice(), data_start_location, end_location.clone()),
                    ),
                    start_location.start().clone(),
                    end_location,
                ));
            }
            Token::Directive("signature") => {
                let symbol = parse_symbol(&mut state);

                match state.input.next_token() {
                    Some(((Token::Word("type"), _), location)) => {
                        let signature_start_location;
                        let end_location;
                        let type_signature = match state.input.next_token() {
                            Some(((Token::Word("rawptr"), _), pointer_type_location)) => {
                                let mut pointee_type = None;

                                parse_reference(
                                    &mut state,
                                    |_, pt| pointee_type = Some(pt),
                                    |state, token| {
                                        state.push_error(
                                            ErrorKind::InvalidPointeeType,
                                            token
                                                .map(|(_, l)| l.clone())
                                                .unwrap_or_else(|| pointer_type_location.end().clone().into()),
                                        )
                                    },
                                );

                                if let Some(pointee_type) = pointee_type {
                                    signature_start_location = pointer_type_location.start().clone();
                                    end_location = pointee_type.location().end().clone();
                                    ast::TypeSignature::RawPtr(pointee_type)
                                } else {
                                    continue;
                                }
                            }
                            Some(((Token::Word("voidptr"), _), location)) => {
                                signature_start_location = location.start().clone();
                                end_location = location.end().clone();
                                ast::TypeSignature::VoidPtr
                            }
                            Some(((Token::Word(primitive_type_name), _), location)) => {
                                signature_start_location = location.start().clone();
                                end_location = location.end().clone();
                                match *primitive_type_name {
                                    "u8" => ast::TypeSignature::U8,
                                    "s8" => ast::TypeSignature::S8,
                                    "u16" => ast::TypeSignature::U16,
                                    "s16" => ast::TypeSignature::S16,
                                    "u32" => ast::TypeSignature::U32,
                                    "s32" => ast::TypeSignature::S32,
                                    "u64" => ast::TypeSignature::U64,
                                    "s64" => ast::TypeSignature::S64,
                                    "uaddr" => ast::TypeSignature::UAddr,
                                    "saddr" => ast::TypeSignature::SAddr,
                                    "f32" => ast::TypeSignature::F32,
                                    "f64" => ast::TypeSignature::F64,
                                    bad => {
                                        state.push_error(ErrorKind::UnknownPrimitiveType(Box::from(bad)), location);
                                        state.input.skip_current_line();
                                        continue;
                                    }
                                }
                            }
                            bad => {
                                state.push_error(
                                    ErrorKind::InvalidTypeSignature,
                                    bad.map(|(_, location)| location.clone())
                                        .unwrap_or_else(|| location.end().into()),
                                );
                                state.input.skip_current_line();
                                continue;
                            }
                        };

                        state.output.tree.push(ast::Located::new(
                            ast::Directive::Signature(
                                symbol,
                                ast::Located::new(
                                    ast::Signature::Type(type_signature),
                                    signature_start_location,
                                    end_location.clone(),
                                ),
                            ),
                            start_location.start().clone(),
                            end_location,
                        ));
                    }
                    Some(((Token::Word(invalid), _), location)) => {
                        state.push_error(ErrorKind::InvalidSignatureKind(Box::from(*invalid)), location);
                        state.input.skip_current_line();
                    }
                    bad => {
                        state.push_error(
                            ErrorKind::ExpectedSignatureKind,
                            token_location_or_last(bad.as_ref(), state.locations()),
                        );
                        state.input.skip_current_line();
                    }
                }

                state.expect_newline_or_end();
            }
            Token::Directive(unknown) => {
                state.push_error(ErrorKind::UnknownDirective(Box::from(*unknown)), start_location);
                state.input.skip_current_line();
                continue;
            }
            Token::Unknown | _ => state.push_error(ErrorKind::UnknownToken, start_location),
        }
    }

    state.output
}
