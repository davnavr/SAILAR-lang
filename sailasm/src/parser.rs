//! Provides functions for parsing SAILAR assembly.

use crate::ast;
use crate::lexer::{self, Token};
use std::iter::Iterator;
use std::ops::Range;

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("unknown token")]
    UnknownToken,
    #[error("expected symbol or integer index")]
    ExpectedReference,
    #[error("\"\\{0}\" is not a valid escape sequence")]
    InvalidEscapeSequence(Box<str>),
    #[error("invalid integer literal: {0}")]
    InvalidIntegerLiteral(std::num::ParseIntError),
    #[error("expected end of line or file")]
    ExpectedNewLineOrEndOfFile,
    #[error("unknown directive .{0}")]
    UnknownDirective(Box<str>),
    #[error("unknown nested directive /{0}")]
    UnknownNestedDirective(Box<str>),
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
    #[error("expected byte literals in data declaration")]
    ExpectedDataByteLiteral,
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
    #[error("expected list of type symbols or indices for argument and return types")]
    ExpectedFunctionSignatureTypeList,
    #[error("expected register symbol and/or type")]
    ExpectedRegisterDeclaration,
    #[error("expected type for register")]
    ExpectedRegisterType,
    #[error("expected return types of code block")]
    ExpectedCodeBlockReturnTypes,
    #[error("expected closing parenthesis")]
    ExpectedClosingParenthesis,
    #[error("{0} is not a known instruction")]
    UnknownInstruction(Box<str>),
    #[error("expected instruction")]
    ExpectedInstruction,
    #[error("expected register or constant value")]
    ExpectedInstructionValue,
    #[error("expected '=' before instruction")]
    ExpectedStatementEqualsSign,
    #[error("expected definition kind (e.g. function, struct, global)")]
    ExpectedDefinitionKind,
    #[error("{0} is not a valid definition kind")]
    InvalidDefinitionKind(Box<str>),
    #[error("expected function signature")]
    ExpectedFunctionSignature,
    #[error("expected foreign function body or /body directives")]
    ExpectedFunctionDefinitionBody,
    #[error("expected identifier string indicating name of foreign function")]
    ExpectedForeignFunctionIdentifier,
    #[error("{0} is not a valid instantiation kind")]
    InvalidInstantiationKind(Box<str>),
    #[error("expected instantiation kind")]
    ExpectedInstantiationKind,
    #[error("expected definition or import")]
    ExpectedDefinitionOrImport,
}

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
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
    reference_buffer: Vec<ast::Reference<'source>>,
    typed_register_buffer: Vec<ast::Located<ast::TypedRegister<'source>>>,
    statement_buffer: Vec<ast::Statement<'source>>,
    output: Output<'source>,
    instruction_value_buffer: Vec<ast::Value<'source>>,
}

impl<'tree, 'source> State<'tree, 'source> {
    fn locations(&self) -> &'tree lexer::OffsetMap<'source> {
        self.input.locations
    }

    fn push_error<E: Into<ErrorKind>, L: Into<ast::LocationRange>>(&mut self, error: E, location: L) {
        self.output.errors.push(Error::new(error, location));
    }

    fn expect_newline_or_end(&mut self) -> ast::Location {
        match self.input.peek_next_token() {
            Some(((Token::Newline, _), location)) => location.end().clone(),
            Some((_, location)) => {
                let start_location = location.start();
                let mut end_location = location.end().clone();

                while let Some((_, location)) = self.input.next_token_if(|next| !matches!(next, Token::Newline)) {
                    end_location = location.end().clone();
                }

                self.push_error(
                    ErrorKind::ExpectedNewLineOrEndOfFile,
                    ast::LocationRange::new(start_location.clone(), end_location.clone()),
                );

                end_location
            }
            None => self.locations().get_last().unwrap(),
        }
    }
}

fn token_location_or_else(token: Option<&LocatedToken>, default: impl FnOnce() -> ast::LocationRange) -> ast::LocationRange {
    token.map(|(_, location)| location.clone()).unwrap_or_else(default)
}

fn token_location_or_last(token: Option<&LocatedToken>, locations: &lexer::OffsetMap) -> ast::LocationRange {
    token_location_or_else(token, || locations.get_last().unwrap().into())
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

fn parse_label<'t, 's>(state: &mut State<'t, 's>) -> Option<ast::Symbol<'s>> {
    state.input.peek_next_token().and_then(|token| match token {
        ((Token::Label(label), _), location) => {
            state.input.next_token();
            Some(ast::Located::with_range(*label, location))
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
        Some(((Token::Label(label), _), location)) => {
            success(state, ast::Reference::Label(ast::Located::with_range(*label, location)))
        }
        Some(((Token::Index(digits), _), location)) => match u32::try_from(digits) {
            Ok(index) => success(state, ast::Reference::Index(ast::Located::with_range(index, location))),
            Err(e) => state.push_error(ErrorKind::InvalidIntegerLiteral(e), location),
        },
        bad => exhausted(state, bad),
    }
}

/// Parses a comma separated list of register symbols with type annotations, appending the parsed registers to the
/// `typed_register_buffer` of the `state`.
fn parse_typed_register_list<'t, 's>(state: &mut State<'t, 's>) -> Box<[ast::Located<ast::TypedRegister<'s>>]> {
    state.typed_register_buffer.clear();

    loop {
        let register_symbol;
        let start_location;
        match state.input.peek_next_token() {
            Some(((Token::Register(name), _), location)) => {
                start_location = location.start().clone();
                register_symbol = Some(ast::Symbol::with_range(name, location));
            }
            Some(((Token::Underscore, _), location)) => {
                register_symbol = None;
                start_location = location.start().clone();
            }
            bad => {
                if !state.typed_register_buffer.is_empty() {
                    state.push_error(
                        ErrorKind::ExpectedRegisterDeclaration,
                        token_location_or_last(bad.as_ref(), state.locations()),
                    );
                }

                break;
            }
        }

        state.input.next_token();

        match state.input.peek_next_token() {
            Some(((Token::Colon, _), _)) => {
                state.input.next_token();
            }
            bad => {
                state.push_error(
                    ErrorKind::ExpectedRegisterType,
                    token_location_or_last(bad.as_ref(), state.locations()),
                );

                break;
            }
        }

        parse_reference(
            state,
            |state, register_type| {
                let end_location = register_type.location().end().clone();

                state.typed_register_buffer.push(ast::Located::new(
                    ast::TypedRegister::new(register_symbol, register_type),
                    start_location,
                    end_location,
                ));
            },
            |state, bad| {
                state.push_error(
                    ErrorKind::ExpectedRegisterType,
                    token_location_or_last(bad.as_ref(), state.locations()),
                )
            },
        );

        if let Some(((Token::Comma, _), _)) = state.input.peek_next_token() {
            state.input.next_token();
            continue;
        } else {
            break;
        }
    }

    state.typed_register_buffer.clone().into_boxed_slice()
}

fn parse_instruction_value<'t, 's>(state: &mut State<'t, 's>) -> Option<ast::Value<'s>> {
    match state.input.peek_next_token() {
        Some(((Token::Register(register), _), location)) => {
            state.input.next_token();
            Some(ast::Value::Register(ast::Reference::Label(ast::Symbol::with_range(
                register, location,
            ))))
        }
        Some(((Token::LiteralInteger(integer), _), location)) => {
            state.input.next_token();
            Some(ast::Value::LiteralInteger(ast::Located::with_range(
                integer.clone(),
                location,
            )))
        }
        _ => None,
    }
}

fn parse_instruction_value_list<'t, 's>(state: &mut State<'t, 's>) -> Box<[ast::Value<'s>]> {
    state.instruction_value_buffer.clear();

    loop {
        if let Some(value) = parse_instruction_value(state) {
            state.instruction_value_buffer.push(value);

            if state.input.next_token_if(|t| matches!(t, Token::Comma)).is_none() {
                break;
            }
        } else if !state.instruction_value_buffer.is_empty() {
            let location = match state.input.peek_next_token() {
                Some(((token, _), location)) => {
                    match token {
                        Token::Newline => (),
                        _ => {
                            state.input.next_token();
                        }
                    }

                    location.clone()
                }
                None => state.locations().get_last().unwrap().into(),
            };

            state.push_error(ErrorKind::ExpectedInstructionValue, location);
        } else {
            break;
        }
    }

    state.instruction_value_buffer.clone().into_boxed_slice()
}

fn parse_access_modifier<'t, 's>(state: &mut State<'t, 's>) -> ast::Export {
    match state.input.peek_next_token() {
        Some(((Token::Word("public"), _), _)) => {
            state.input.next_token();
            ast::Export::Public
        }
        Some(((Token::Word("private"), _), _)) => {
            state.input.next_token();
            ast::Export::Private
        }
        _ => ast::Export::default(),
    }
}

fn parse_definition_or_import<'t, 's>(
    state: &mut State<'t, 's>,
    success: impl FnOnce(&mut State<'t, 's>, ast::DefinitionOrImport<'s>),
) {
    match state.input.next_token() {
        Some(((Token::Word(kind @ ("definition" | "def" | "import" | "imp")), _), _)) => parse_reference(
            state,
            |state, reference| {
                let symbol = match *kind {
                    "import" | "imp" => ast::DefinitionOrImport::Import(reference),
                    _ => ast::DefinitionOrImport::Definition(reference),
                };

                success(state, symbol);
            },
            |state, bad| {
                state.push_error(
                    ErrorKind::ExpectedReference,
                    token_location_or_last(bad.as_ref(), state.locations()),
                );
            },
        ),
        bad => {
            state.push_error(
                ErrorKind::ExpectedDefinitionOrImport,
                token_location_or_last(bad.as_ref(), state.locations()),
            );
        }
    }
}

/// Transfers a sequence of tokens into an abstract syntax tree.
pub fn parse<'source>(input: &lexer::Output<'source>) -> Output<'source> {
    let mut state = State {
        input: SliceInput::new(input.tokens(), input.locations()),
        character_buffer: String::default(),
        reference_buffer: Vec::default(),
        typed_register_buffer: Vec::default(),
        statement_buffer: Vec::default(),
        instruction_value_buffer: Vec::default(),
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
            Token::Directive("metadata" | "meta") => match state.input.next_token() {
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
            Token::Directive("identifier" | "ident") => {
                let symbol = parse_label(&mut state);

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
                let symbol = parse_label(&mut state);
                let mut data = Vec::default();
                let mut end_location = start_location.end().clone();
                let mut data_start_location = end_location.clone();
                let mut data_location_updated = false;
                let mut is_start_of_line = false;

                while let Some(((token, _), location)) = state.input.peek_next_token() {
                    match token {
                        Token::Newline => is_start_of_line = true,
                        Token::Directive(_) if is_start_of_line => {
                            break;
                        }
                        Token::LiteralInteger(digits) => match u8::try_from(digits) {
                            Ok(byte) => data.push(byte),
                            Err(e) => state.push_error(ErrorKind::InvalidByteLiteral(e), location.clone()),
                        },
                        _ => state.push_error(ErrorKind::ExpectedDataByteLiteral, location.clone()),
                    }

                    if !data_location_updated {
                        data_start_location = location.start().clone();
                        data_location_updated = true;
                    }

                    end_location = location.end().clone();

                    state.input.next_token();
                }

                if !is_start_of_line {
                    state.expect_newline_or_end();
                }

                state.output.tree.push(ast::Located::new(
                    ast::Directive::Data(
                        symbol,
                        ast::Located::new(data.into_boxed_slice(), data_start_location, end_location.clone()),
                    ),
                    start_location.start().clone(),
                    end_location,
                ));
            }
            Token::Directive("signature" | "sig") => {
                let symbol = parse_label(&mut state);

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
                                                .map(|(_, l)| l)
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
                                    bad.map(|(_, location)| location).unwrap_or_else(|| location.end().into()),
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
                    Some(((Token::Word("function" | "func"), _), location)) => {
                        fn function_types<'t, 's>(
                            state: &mut State<'t, 's>,
                            signature_location: &ast::Location,
                            reference_buffer: &mut Vec<ast::Reference<'s>>,
                            success: impl FnOnce(&mut State<'t, 's>, &mut Vec<ast::Reference<'s>>, ast::LocationRange),
                        ) {
                            let start_location;
                            let mut end_location;
                            match state.input.next_token() {
                                Some(((Token::OpenParenthesis, _), location)) => {
                                    start_location = location.start().clone();
                                    end_location = std::cell::RefCell::new(location.end().clone());
                                }
                                bad => {
                                    state.push_error(
                                        ErrorKind::ExpectedFunctionSignatureTypeList,
                                        token_location_or_else(bad.as_ref(), || {
                                            ast::LocationRange::from(signature_location.clone())
                                        }),
                                    );
                                    return;
                                }
                            }

                            let mut parse_type_reference = |state: &mut State<'t, 's>| {
                                parse_reference(
                                    state,
                                    |_, s| {
                                        *end_location.borrow_mut() = s.location().end().clone();
                                        reference_buffer.push(s);
                                    },
                                    |s, b| {
                                        s.push_error(
                                            ErrorKind::ExpectedReference,
                                            token_location_or_else(b.as_ref(), || ast::LocationRange::from(end_location.clone())),
                                        )
                                    },
                                )
                            };

                            parse_type_reference(state);

                            while state.input.next_token_if(|t| matches!(t, Token::Comma)).is_some() {
                                parse_type_reference(state);
                            }

                            match state.input.next_token() {
                                Some(((Token::CloseParenthesis, _), location)) => {
                                    *end_location.get_mut() = location.end().clone();
                                }
                                bad => {
                                    state.push_error(
                                        ErrorKind::ExpectedReference,
                                        token_location_or_else(bad.as_ref(), || {
                                            ast::LocationRange::from(end_location.into_inner())
                                        }),
                                    );
                                    return;
                                }
                            }

                            success(
                                state,
                                reference_buffer,
                                ast::LocationRange::new(start_location, end_location.into_inner()),
                            )
                        }

                        let mut reference_buffer = std::mem::take(&mut state.reference_buffer);
                        reference_buffer.clear();

                        let start_location = location.end().clone();

                        function_types(
                            &mut state,
                            &start_location,
                            &mut reference_buffer,
                            |state, reference_buffer, parameter_types_location| {
                                let parameter_type_count = reference_buffer.len();
                                let mut result_types_location = None;
                                if state.input.next_token_if(|t| matches!(t, Token::ResultSymbol)).is_some() {
                                    function_types(state, parameter_types_location.end(), reference_buffer, |_, _, l| {
                                        result_types_location = Some(l);
                                    });
                                }

                                state.expect_newline_or_end();

                                let signature_start_location = parameter_types_location.start().clone();

                                let signature_end_location =
                                    result_types_location.unwrap_or(parameter_types_location).end().clone();

                                state.output.tree.push(ast::Located::new(
                                    ast::Directive::Signature(
                                        symbol,
                                        ast::Located::new(
                                            ast::Signature::Function(ast::FunctionSignature::from_vec(
                                                reference_buffer,
                                                parameter_type_count,
                                            )),
                                            signature_start_location,
                                            signature_end_location.clone(),
                                        ),
                                    ),
                                    start_location.clone(),
                                    signature_end_location,
                                ));
                            },
                        );

                        state.reference_buffer = reference_buffer;
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
            Token::Directive("code") => {
                let symbol = parse_label(&mut state);

                let input_registers;
                if state.input.next_token_if(|t| matches!(t, Token::OpenParenthesis)).is_some() {
                    input_registers = parse_typed_register_list(&mut state);

                    match state.input.peek_next_token() {
                        Some(((Token::CloseParenthesis, _), _)) => {
                            state.input.next_token();
                        }
                        bad => state.push_error(
                            ErrorKind::ExpectedClosingParenthesis,
                            token_location_or_last(bad.as_ref(), state.locations()),
                        ),
                    }
                } else {
                    input_registers = Box::default();
                };

                let result_types = match state.input.peek_next_token() {
                    Some(((Token::ResultSymbol, _), _)) => {
                        state.input.next_token();
                        todo!("parse result types of code block")
                    }
                    Some(((Token::Newline, _), _)) | None => {
                        // Do not consume newline token or EOF, as following call expects a newline or EOF.
                        Box::default()
                    }
                    Some((_, location)) => {
                        state.push_error(ErrorKind::ExpectedCodeBlockReturnTypes, location);
                        state.input.next_token();
                        Box::default()
                    }
                };

                let mut end_location = state.expect_newline_or_end();

                state.statement_buffer.clear();

                let mut is_start_of_line = true;
                loop {
                    assert!(is_start_of_line, "statements must begin at the start of a line");

                    let temporary_registers = parse_typed_register_list(&mut state);

                    if !temporary_registers.is_empty() {
                        match state.input.peek_next_token() {
                            Some(((Token::EqualsSign, _), location)) => {
                                state.input.next_token();
                                end_location = location.end().clone();
                            }
                            bad => {
                                state.push_error(
                                    ErrorKind::ExpectedStatementEqualsSign,
                                    token_location_or_last(bad.as_ref(), state.locations()),
                                );
                            }
                        }
                    }

                    match state.input.peek_next_token() {
                        Some(((Token::Word(instruction_name), _), location)) => {
                            state.input.next_token();
                            end_location = location.end().clone();

                            let instruction = match *instruction_name {
                                "nop" => ast::Instruction::Nop,
                                "break" => ast::Instruction::Break,
                                "ret" => ast::Instruction::Ret(parse_instruction_value_list(&mut state)),
                                unknown => {
                                    state.push_error(ErrorKind::UnknownInstruction(Box::from(unknown)), location.clone());
                                    ast::Instruction::Nop
                                }
                            };

                            state.statement_buffer.push(ast::Statement::new(
                                temporary_registers,
                                ast::Located::new(instruction, location.start().clone(), end_location.clone()),
                            ));

                            state.expect_newline_or_end();
                            is_start_of_line = true;
                        }
                        Some(((Token::Newline, _), location)) => {
                            if !temporary_registers.is_empty() {
                                state.push_error(ErrorKind::ExpectedInstruction, location.clone());
                            }

                            state.input.next_token();
                            end_location = location.end().clone();
                            is_start_of_line = true;
                        }
                        Some(((Token::Directive(_), _), location)) => {
                            if !is_start_of_line {
                                end_location = location.end().clone();
                                state.input.next_token();
                                state.expect_newline_or_end();
                            }

                            break;
                        }
                        Some((_, location)) => {
                            end_location = location.end().clone();
                            state.push_error(
                                if is_start_of_line {
                                    ErrorKind::ExpectedInstruction
                                } else {
                                    ErrorKind::UnknownToken
                                },
                                location,
                            );
                            state.input.next_token();
                            state.input.skip_current_line();
                            break;
                        }
                        None => {
                            if !temporary_registers.is_empty() {
                                state.push_error(ErrorKind::ExpectedInstruction, state.locations().get_last().unwrap());
                            }

                            state.input.next_token();
                            break;
                        }
                    }
                }

                state.output.tree.push(ast::Located::new(
                    ast::Directive::Code(
                        symbol,
                        ast::CodeBlock::new(
                            input_registers,
                            result_types,
                            state.statement_buffer.clone().into_boxed_slice(),
                        ),
                    ),
                    start_location.start().clone(),
                    end_location,
                ));
            }
            Token::Directive("define" | "def") => {
                let symbol = parse_label(&mut state);

                match state.input.next_token() {
                    Some(((Token::Word("function" | "func"), _), _)) => {
                        let access_modifier = parse_access_modifier(&mut state);

                        parse_literal_identifier(
                            &mut state,
                            move |state, identifier| {
                                match state.input.peek_next_token() {
                                    Some(((Token::Word("signature"), _), _)) => {
                                        state.input.next_token();
                                    }
                                    bad => {
                                        state.push_error(
                                            ErrorKind::ExpectedFunctionSignature,
                                            token_location_or_last(bad.as_ref(), state.locations()),
                                        );
                                        state.input.skip_current_line();
                                        return;
                                    }
                                }

                                parse_reference(
                                    state,
                                    |state, signature| match state.input.peek_next_token() {
                                        Some(((Token::Newline, _), location)) => {
                                            let mut end_location = location.end().clone();

                                            state.input.next_token();
                                            state.reference_buffer.clear();

                                            loop {
                                                match state.input.peek_next_token() {
                                                    Some(((Token::NestedDirective("body"), _), location)) => {
                                                        state.input.next_token();
                                                        end_location = location.end().clone();

                                                        parse_reference(
                                                            state,
                                                            |state, entry_block| {
                                                                state.reference_buffer.push(entry_block);
                                                            },
                                                            |state, bad| {
                                                                state.push_error(
                                                                    ErrorKind::ExpectedReference,
                                                                    token_location_or_last(bad.as_ref(), state.locations()),
                                                                )
                                                            },
                                                        );

                                                        state.input.skip_current_line();
                                                    }
                                                    Some(((Token::Newline, _), location)) => {
                                                        state.input.next_token();
                                                        end_location = location.end().clone();
                                                    }
                                                    Some(((Token::Directive(_), _), _)) => {
                                                        break;
                                                    }
                                                    Some(((unknown, _), location)) => {
                                                        let error = match unknown {
                                                            Token::NestedDirective(bad) => {
                                                                ErrorKind::UnknownNestedDirective(Box::from(*bad))
                                                            }
                                                            _ => ErrorKind::ExpectedFunctionDefinitionBody,
                                                        };

                                                        state.push_error(error, location);
                                                        state.input.next_token();
                                                        state.input.skip_current_line();
                                                    }
                                                    None => {
                                                        state.input.next_token();
                                                        break;
                                                    }
                                                }
                                            }

                                            state.output.tree.push(ast::Located::new(
                                                ast::Directive::FunctionDefinition(
                                                    symbol,
                                                    ast::FunctionDefinition::new(
                                                        access_modifier,
                                                        identifier,
                                                        signature,
                                                        ast::FunctionBody::Defined(
                                                            state.reference_buffer.clone().into_boxed_slice(),
                                                        ),
                                                    ),
                                                ),
                                                start_location.start().clone(),
                                                end_location,
                                            ));
                                        }
                                        Some(((Token::Word("foreign"), _), _)) => {
                                            state.input.next_token();

                                            parse_literal_identifier(
                                                state,
                                                |state, function_name| todo!("foreign function bodies are not yet supported"),
                                                |state, bad| {
                                                    state.push_error(
                                                        ErrorKind::ExpectedForeignFunctionIdentifier,
                                                        token_location_or_last(bad.as_ref(), state.locations()),
                                                    );
                                                    state.input.skip_current_line();
                                                },
                                            );
                                        }
                                        bad => {
                                            state.push_error(
                                                ErrorKind::ExpectedFunctionDefinitionBody,
                                                token_location_or_last(bad.as_ref(), state.locations()),
                                            );
                                            state.input.skip_current_line();
                                        }
                                    },
                                    |state, bad| {
                                        state.push_error(
                                            ErrorKind::ExpectedReference,
                                            token_location_or_last(bad.as_ref(), state.locations()),
                                        );
                                        state.input.skip_current_line();
                                    },
                                );
                            },
                            |state, bad| {
                                state.push_error(
                                    ErrorKind::ExpectedIdentifierLiteral,
                                    token_location_or_last(bad.as_ref(), state.locations()),
                                )
                            },
                        );
                    }
                    bad => {
                        let error = match bad {
                            Some(((Token::Word(unknown), _), _)) => ErrorKind::InvalidDefinitionKind(Box::from(*unknown)),
                            _ => ErrorKind::ExpectedDefinitionKind,
                        };

                        state.push_error(error, token_location_or_last(bad.as_ref(), state.locations()));
                        state.input.skip_current_line();
                        continue;
                    }
                }
            }
            Token::Directive("instantiate" | "inst") => {
                let symbol = parse_label(&mut state);

                match state.input.next_token() {
                    Some(((Token::Word("function" | "func"), _), location)) => {
                        let start_location = location.start().clone();
                        parse_definition_or_import(&mut state, |state, instantiation| {
                            state.expect_newline_or_end();
                            let end_location = instantiation.location().end().clone();
                            state.output.tree.push(ast::Located::new(
                                ast::Directive::FunctionInstantiation(symbol, instantiation),
                                start_location,
                                end_location,
                            ));
                        })
                    }
                    bad => {
                        let error = if let Some(((Token::Word(unknown), _), _)) = bad {
                            ErrorKind::InvalidInstantiationKind(Box::from(*unknown))
                        } else {
                            ErrorKind::ExpectedInstantiationKind
                        };

                        state.push_error(error, token_location_or_last(bad.as_ref(), state.locations()));
                        state.input.skip_current_line();
                    }
                }
            }
            Token::Directive(unknown) => {
                state.push_error(ErrorKind::UnknownDirective(Box::from(*unknown)), start_location);
                state.input.skip_current_line();
                continue;
            }
            Token::NestedDirective(unknown) => {
                state.push_error(ErrorKind::UnknownNestedDirective(Box::from(*unknown)), start_location);
                state.input.skip_current_line();
                continue;
            }
            _ => state.push_error(ErrorKind::UnknownToken, start_location),
        }
    }

    state.output
}

#[cfg(test)]
mod tests {
    #[test]
    fn data_record_allows_bytes_on_newline() {
        let tokens = crate::lexer::tokenize("\n.data 1 2\n3 4\n.identifier \"test\"\n");
        let tree = crate::parser::parse(&tokens);
        let empty: &[crate::parser::Error] = &[];
        assert_eq!(empty, tree.errors());
    }

    #[test]
    fn basic_code_record_example() {
        let tokens = crate::lexer::tokenize("\n.code ($a0:#1, $a1:#2)\nnop\nbreak\n$t0:@thing, $t1:#1 = nop\n.data 1\n");
        let tree = crate::parser::parse(&tokens);
        let empty: &[crate::parser::Error] = &[];
        assert_eq!(empty, tree.errors());
    }
}
