//! Provides functions for the tokenization of SAILAR assembly.

use crate::ast;
use logos::Logos;
use std::fmt::{Debug, Formatter};
use std::ops::Range;

#[derive(Debug)]
pub struct OffsetMapBuilder<'s> {
    lookup: Vec<(usize, ast::LocationNumber, &'s str)>,
    input: &'s str,
    previous_offset: usize,
    next_line_number: ast::LocationNumber,
}

impl<'s> OffsetMapBuilder<'s> {
    fn new(input: &'s str) -> Self {
        Self {
            lookup: Vec::with_capacity(16),
            input,
            previous_offset: 0,
            next_line_number: ast::LocationNumber::new(1).unwrap(),
        }
    }

    fn push_new_line(&mut self, offset: usize) {
        self.lookup.push((
            self.previous_offset,
            self.next_line_number,
            &self.input[self.previous_offset..offset],
        ));
        self.previous_offset = offset + 1;
        self.next_line_number = ast::LocationNumber::new(self.next_line_number.get() + 1).unwrap();
    }

    fn finish(mut self) -> OffsetMap<'s> {
        let last_offset = self.input.len();

        match self.lookup.last() {
            Some((last, _, _)) if *last < last_offset => self.push_new_line(self.input.len()),
            _ => (),
        };

        OffsetMap {
            bytes: 0..last_offset,
            lookup: self.lookup,
        }
    }
}

/// Maps byte offsets into the input file into line and column numbers.
#[derive(Clone, Debug)]
pub struct OffsetMap<'s> {
    bytes: Range<usize>,
    lookup: Vec<(usize, ast::LocationNumber, &'s str)>,
}

pub struct OffsetMapLocations<'m, 's> {
    bytes: Range<usize>,
    lookup: &'m OffsetMap<'s>,
}

impl<'s> std::iter::Iterator for OffsetMapLocations<'_, 's> {
    type Item = (usize, ast::Location);

    fn next(&mut self) -> Option<Self::Item> {
        let offset = self.bytes.next()?;
        Some((offset, self.lookup.get_location(offset)?))
    }
}

impl std::iter::ExactSizeIterator for OffsetMapLocations<'_, '_> {
    fn len(&self) -> usize {
        self.bytes.len()
    }
}

impl<'s> OffsetMap<'s> {
    pub fn get_location(&self, offset: usize) -> Option<ast::Location> {
        if !self.lookup.is_empty() {
            match self.lookup.binary_search_by_key(&offset, |(o, _, _)| *o) {
                Ok(exact) => Some(ast::Location::new(self.lookup[exact].1, ast::LOCATION_NUMBER_START)),
                Err(0) => unreachable!("missing first line lookup entry"),
                Err(index) => {
                    let (line_offset, line_number, line) = self.lookup[index - 1];
                    let column_count = line[0..offset - line_offset].chars().count();
                    let column_number = ast::LocationNumber::new(column_count + 1).expect("valid column number");
                    Some(ast::Location::new(line_number, column_number))
                }
            }
        } else {
            None
        }
    }

    pub(crate) fn get_last(&self) -> Option<ast::Location> {
        self.lookup
            .last()
            .map(|(_, line_number, _)| ast::Location::new(*line_number, ast::LOCATION_NUMBER_START))
    }

    /// Returns an iterator over each byte offset in the input and the corresponding line and column number.
    pub fn iter_locations(&self) -> OffsetMapLocations<'_, 's> {
        OffsetMapLocations {
            bytes: self.bytes.clone(),
            lookup: self,
        }
    }
}

fn literal_string_contents<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> &'s str {
    let token: &'s str = lex.slice();
    &token[1..token.len() - 1]
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntegerLiteralBase {
    Binary,
    Decimal,
    Hexadecimal,
}

impl IntegerLiteralBase {
    pub fn radix(self) -> u8 {
        match self {
            Self::Binary => 2,
            Self::Decimal => 10,
            Self::Hexadecimal => 16,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct LiteralDigits<'s> {
    base: IntegerLiteralBase,
    digits: &'s str,
}

impl LiteralDigits<'_> {
    #[inline]
    pub fn base(&self) -> IntegerLiteralBase {
        self.base
    }

    #[inline]
    pub fn digits(&self) -> &str {
        self.digits
    }

    fn to_integer_radix<T>(
        &self,
        convert: fn(&str, u32) -> Result<T, std::num::ParseIntError>,
    ) -> Result<T, std::num::ParseIntError> {
        convert(self.digits, self.base.radix().into())
    }
}

impl TryFrom<&LiteralDigits<'_>> for u8 {
    type Error = std::num::ParseIntError;

    fn try_from(digits: &LiteralDigits<'_>) -> Result<u8, Self::Error> {
        digits.to_integer_radix(u8::from_str_radix)
    }
}

impl Debug for LiteralDigits<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.base {
            IntegerLiteralBase::Binary => f.write_str("0b")?,
            IntegerLiteralBase::Hexadecimal => f.write_str("0x")?,
            IntegerLiteralBase::Decimal => (),
        }

        f.write_str(self.digits)
    }
}

fn literal_integer_contents<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Option<LiteralDigits<'s>> {
    let token = lex.slice();
    let base;
    let digits;

    match token.get(0..2) {
        Some("0x" | "0X") => {
            base = IntegerLiteralBase::Binary;
            digits = &token[2..];
        }
        Some("0b" | "0B") => {
            base = IntegerLiteralBase::Hexadecimal;
            digits = &token[2..];
        }
        _ => {
            base = IntegerLiteralBase::Decimal;
            digits = token;
        }
    }

    Some(LiteralDigits { base, digits })
}

fn unknown_directive<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> &'s str {
    &lex.slice()[1..]
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = OffsetMapBuilder<'s>)]
pub enum Token<'s> {
    #[token(".array")]
    ArrayDirective,
    #[token(".format")]
    FormatDirective,
    #[token(".metadata")]
    MetadataDirective,
    #[token(".identifier")]
    IdentifierDirective,
    #[regex(r"[a-zA-Z][a-zA-Z_0-9]*")]
    Word(&'s str),
    #[regex("\"[a-zA-Z0-9_ \\?\\\\/!\\*\\+\\.]*\"", literal_string_contents)]
    LiteralString(&'s str),
    #[regex("(0[Bb][01][01_]*)|(0[Xx][0-9a-fA-F][0-9a-fA-F_]*)|[0-9][0-9_]*", literal_integer_contents)]
    LiteralInteger(LiteralDigits<'s>),
    #[regex(r"\n|\r|(\r\n)")]
    Newline,
    #[regex(r"\.[a-zA-Z]+", unknown_directive)]
    UnknownDirective(&'s str),
    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r";[ \t\w\d;\?!\\/\.\*\+-=#]*", logos::skip)]
    Unknown,
}

#[derive(Debug)]
pub struct Output<'s> {
    tokens: Vec<(Token<'s>, Range<usize>)>,
    offset_map: OffsetMap<'s>,
}

impl<'s> Output<'s> {
    #[inline]
    pub fn tokens(&self) -> &[(Token<'s>, Range<usize>)] {
        &self.tokens
    }

    #[inline]
    pub fn locations(&self) -> &OffsetMap<'s> {
        &self.offset_map
    }
}

pub fn tokenize(input: &str) -> Output<'_> {
    let mut tokens = Vec::default();
    let mut lexer = Token::lexer_with_extras(input, OffsetMapBuilder::new(input));
    while let Some(token) = lexer.next() {
        let offset = lexer.span();

        if let Token::Newline = token {
            lexer.extras.push_new_line(offset.start);
        }

        tokens.push((token, offset));
    }

    Output {
        tokens,
        offset_map: lexer.extras.finish(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_actual_locations(lookup: &OffsetMap) -> Vec<(usize, usize, usize)> {
        lookup
            .iter_locations()
            .map(|(byte_offset, location)| (byte_offset, location.line.get(), location.column.get()))
            .collect()
    }

    #[test]
    fn format_version_is_tokenized() {
        let output = tokenize(".format major 0\n.format minor 12 ; Comment\n");

        let expected_tokens = [
            (Token::FormatDirective, 0usize..7),
            (Token::Word("major"), 8..13),
            (
                Token::LiteralInteger(LiteralDigits {
                    base: IntegerLiteralBase::Decimal,
                    digits: "0",
                }),
                14..15,
            ),
            (Token::Newline, 15..16),
            (Token::FormatDirective, 16..23),
            (Token::Word("minor"), 24..29),
            (
                Token::LiteralInteger(LiteralDigits {
                    base: IntegerLiteralBase::Decimal,
                    digits: "12",
                }),
                30..32,
            ),
            (Token::Newline, 42..43),
        ];

        let expected_locations = [
            (0usize, 1usize, 1usize),
            (1, 1, 2),
            (2, 1, 3),
            (3, 1, 4),
            (4, 1, 5),
            (5, 1, 6),
            (6, 1, 7),
            (7, 1, 8),
            (8, 1, 9),
            (9, 1, 10),
            (10, 1, 11),
            (11, 1, 12),
            (12, 1, 13),
            (13, 1, 14),
            (14, 1, 15),
            (15, 1, 16),
            (16, 2, 1),
            (17, 2, 2),
            (18, 2, 3),
            (19, 2, 4),
            (20, 2, 5),
            (21, 2, 6),
            (22, 2, 7),
            (23, 2, 8),
            (24, 2, 9),
            (25, 2, 10),
            (26, 2, 11),
            (27, 2, 12),
            (28, 2, 13),
            (29, 2, 14),
            (30, 2, 15),
            (31, 2, 16),
            (32, 2, 17),
            (33, 2, 18),
            (34, 2, 19),
            (35, 2, 20),
            (36, 2, 21),
            (37, 2, 22),
            (38, 2, 23),
            (39, 2, 24),
            (40, 2, 25),
            (41, 2, 26),
            (42, 2, 27),
        ];

        assert_eq!(expected_tokens.as_slice(), output.tokens());
        assert_eq!(
            expected_locations.as_slice(),
            collect_actual_locations(output.locations()).as_slice()
        );
    }
}
