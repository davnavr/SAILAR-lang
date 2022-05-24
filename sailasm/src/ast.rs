//! Contains structures to represent the abstract syntax tree.

use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::{Debug, Display, Formatter};

/// Represents a line or column number.
pub type LocationNumber = std::num::NonZeroUsize;

pub(crate) const LOCATION_NUMBER_START: LocationNumber = unsafe {
    // Safety: Constant is guaranteed to be non-zero.
    LocationNumber::new_unchecked(1)
};

/// Represents a pair of line and column numbers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Location {
    pub line: LocationNumber,
    pub column: LocationNumber,
}

impl Location {
    pub const START: Self = Self {
        line: LOCATION_NUMBER_START,
        column: LOCATION_NUMBER_START,
    };

    pub fn new(line: LocationNumber, column: LocationNumber) -> Self {
        Self { line, column }
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.column.cmp(&other.column),
            c => c,
        }
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.column)
    }
}

impl From<Location> for (usize, usize) {
    fn from(location: Location) -> (usize, usize) {
        (location.line.get(), location.column.get())
    }
}

/// An inclusive range of locations in the input.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LocationRange {
    start: Location,
    end: Location,
}

impl LocationRange {
    pub const START: Self = Self {
        start: Location::START,
        end: Location::START,
    };

    pub fn new(start: Location, end: Location) -> Self {
        if start > end {
            panic!("end location {} must not come before start location {}", end, start);
        }

        Self { start, end }
    }

    #[inline]
    pub fn start(&self) -> &Location {
        &self.start
    }

    #[inline]
    pub fn end(&self) -> &Location {
        &self.end
    }
}

impl From<&Location> for LocationRange {
    fn from(location: &Location) -> Self {
        Self::new(location.clone(), location.clone())
    }
}

impl From<Location> for LocationRange {
    fn from(location: Location) -> Self {
        Self::new(location.clone(), location)
    }
}

impl From<std::cell::RefCell<Location>> for LocationRange {
    fn from(location: std::cell::RefCell<Location>) -> Self {
        Self::from(location.into_inner())
    }
}

impl From<&LocationRange> for std::ops::RangeInclusive<Location> {
    fn from(range: &LocationRange) -> Self {
        Self::new(range.start.clone(), range.end.clone())
    }
}

impl Ord for LocationRange {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end.cmp(&other.end),
            c => c,
        }
    }
}

impl PartialOrd for LocationRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for LocationRange {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.start, f)?;
        if self.end > self.start {
            f.write_str(" - ")?;
            Display::fmt(&self.end, f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Located<N> {
    location: LocationRange,
    item: N,
}

impl<N> Located<N> {
    pub fn with_range<L: Into<LocationRange>>(item: N, location: L) -> Self {
        Self {
            item,
            location: location.into(),
        }
    }

    pub fn new(item: N, start: Location, end: Location) -> Self {
        Self::with_range(item, LocationRange::new(start, end))
    }

    #[inline]
    pub fn location(&self) -> &LocationRange {
        &self.location
    }

    #[inline]
    pub fn item(&self) -> &N {
        &self.item
    }

    #[inline]
    pub fn take_item(self) -> N {
        self.item
    }
}

impl<N> From<Located<N>> for (N, LocationRange) {
    fn from(located: Located<N>) -> Self {
        (located.item, located.location)
    }
}

pub type Symbol<'source> = Located<&'source sailar::Id>;

/// Represents a symbol or numeric index used to refer to something.
#[derive(Clone, Debug, PartialEq)]
pub enum Reference<'source> {
    Index(Located<u32>),
    Symbol(Symbol<'source>),
}

impl Reference<'_> {
    pub fn location(&self) -> &LocationRange {
        match self {
            Self::Index(index) => index.location(),
            Self::Symbol(symbol) => symbol.location(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("\"\\{sequence}\" is not a valid escape sequence")]
pub struct InvalidEscapeSequenceError {
    byte_offset: usize,
    sequence: Box<str>,
}

impl InvalidEscapeSequenceError {
    /// The byte offset into the original string where the invalid escape sequence is, where 0 is the first byte in the string
    /// literal.
    #[inline]
    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    /// The invalid escape sequence, without the leading backslash.
    #[inline]
    pub fn sequence(&self) -> &str {
        &self.sequence
    }

    pub(crate) fn take_sequence(self) -> Box<str> {
        self.sequence
    }
}

#[derive(Clone, PartialEq)]
pub struct LiteralString<'source> {
    original_contents: &'source str,
    actual_contents: Option<Box<str>>,
}

pub type Identifier<'source> = Cow<'source, sailar::Id>;

impl<'source> LiteralString<'source> {
    /// Creates a string literal from its contents, which can pontentially contain escape sequences.
    pub fn with_escape_sequences(contents: &'source str, buffer: &mut String) -> Result<Self, InvalidEscapeSequenceError> {
        buffer.clear();

        let mut characters = contents.char_indices();
        while let Some((byte_offset, c)) = characters.next() {
            if c == '\\' {
                if buffer.is_empty() {
                    buffer.reserve(contents.len());
                    buffer.push_str(&contents[0..byte_offset]);
                }

                match characters.next() {
                    Some((_, 't')) => buffer.push('\t'),
                    Some((_, 'n')) => buffer.push('\n'),
                    Some((_, 'r')) => buffer.push('\r'),
                    Some((_, '"')) => buffer.push('"'),
                    Some((_, '\'')) => buffer.push('\''),
                    Some((_, '\\')) => buffer.push('\\'),
                    _ => {
                        return Err(InvalidEscapeSequenceError {
                            byte_offset: byte_offset + 1,
                            sequence: Box::from(characters.as_str()),
                        })
                    }
                }
            } else if !buffer.is_empty() {
                buffer.push(c);
            }
        }

        Ok(Self {
            original_contents: contents,
            actual_contents: if !buffer.is_empty() {
                Some(buffer.clone().into_boxed_str())
            } else {
                None
            },
        })
    }

    /// Gets the contents of the string literal, if it did not contain any escape sequences.
    pub fn as_original_str(&self) -> Result<&'source str, &str> {
        match &self.actual_contents {
            None => Ok(self.original_contents),
            Some(actual_contents) => Err(actual_contents),
        }
    }

    pub fn as_str(&self) -> &str {
        match self.as_original_str() {
            Ok(original) => original,
            Err(actual) => actual,
        }
    }

    pub fn try_into_identifier(self) -> Result<Identifier<'source>, sailar::identifier::InvalidError> {
        match self.actual_contents {
            Some(actual_contents) => sailar::Identifier::try_from(actual_contents).map(Cow::Owned),
            None => sailar::Id::try_from_str(self.original_contents).map(Cow::Borrowed),
        }
    }
}

impl<'source> TryFrom<&'source str> for LiteralString<'source> {
    type Error = InvalidEscapeSequenceError;

    fn try_from(literal: &'source str) -> Result<Self, Self::Error> {
        LiteralString::with_escape_sequences(literal, &mut String::default())
    }
}

impl<'source> TryFrom<LiteralString<'source>> for Identifier<'source> {
    type Error = sailar::identifier::InvalidError;

    #[inline]
    fn try_from(literal: LiteralString<'source>) -> Result<Self, Self::Error> {
        literal.try_into_identifier()
    }
}

impl Debug for LiteralString<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self.original_contents, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FormatVersionKind {
    Major,
    Minor,
}

impl Display for FormatVersionKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Major => "major",
            Self::Minor => "minor",
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Metadata<'source> {
    Identifier(Located<Identifier<'source>>, Box<[u32]>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSignature<'source> {
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    UAddr,
    SAddr,
    F32,
    F64,
    RawPtr(Reference<'source>),
    VoidPtr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSignature<'source> {
    types: Box<[Reference<'source>]>,
    parameter_type_count: usize,
}

impl<'source> FunctionSignature<'source> {
    /// Creates a function signature from the specified types, with the parameter types first and the return types last.
    ///
    /// # Panics
    ///
    /// Panics if the number of parameters exceeds the actual number of types.
    pub(crate) fn from_vec(types: &mut Vec<Reference<'source>>, parameter_type_count: usize) -> Self {
        assert!(parameter_type_count <= types.len());

        Self {
            types: types.clone().into_boxed_slice(),
            parameter_type_count,
        }
    }

    pub fn parameter_types(&self) -> &[Reference<'source>] {
        &self.types[0..self.parameter_type_count]
    }

    pub fn return_types(&self) -> &[Reference<'source>] {
        &self.types[self.parameter_type_count..]
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Signature<'source> {
    Type(TypeSignature<'source>),
    Function(FunctionSignature<'source>),
}

/// Describes a register and the type of the value it contains in a code block.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedRegister<'source> {
    symbol: Option<Symbol<'source>>,
    value_type: Reference<'source>,
}

impl<'source> TypedRegister<'source> {
    pub fn new(symbol: Option<Symbol<'source>>, value_type: Reference<'source>) -> Self {
        Self { symbol, value_type }
    }

    /// Gets the symbol used to refer to this register in the code block, or `None` if only a type is defined for this
    /// register.
    #[inline]
    pub fn symbol(&self) -> Option<&Symbol<'source>> {
        self.symbol.as_ref()
    }

    /// Gets the type of the value contained in this register.
    #[inline]
    pub fn value_type(&self) -> &Reference<'source> {
        &self.value_type
    }
}

pub use crate::lexer::LiteralDigits;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'source> {
    LiteralInteger(Located<LiteralDigits<'source>>),
    Register(Reference<'source>),
}

impl<'source> Value<'source> {
    pub fn location(&self) -> &LocationRange {
        match self {
            Self::LiteralInteger(integer) => integer.location(),
            Self::Register(register) => register.location(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Instruction<'source> {
    Nop,
    Break,
    Ret(Box<[Value<'source>]>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement<'source> {
    results: Box<[Located<TypedRegister<'source>>]>,
    instruction: Located<Instruction<'source>>,
}

impl<'source> Statement<'source> {
    pub fn new(temporary_registers: Box<[Located<TypedRegister<'source>>]>, instruction: Located<Instruction<'source>>) -> Self {
        Self {
            results: temporary_registers,
            instruction,
        }
    }

    /// The temporary registers defined that contain the results of executing the instruction.
    #[inline]
    pub fn results(&self) -> &[Located<TypedRegister<'source>>] {
        &self.results
    }

    #[inline]
    pub fn instruction(&self) -> &Located<Instruction<'source>> {
        &self.instruction
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock<'source> {
    input_registers: Box<[Located<TypedRegister<'source>>]>,
    result_types: Box<[Reference<'source>]>,
    statements: Box<[Statement<'source>]>,
}

impl<'source> CodeBlock<'source> {
    pub fn new(
        input_registers: Box<[Located<TypedRegister<'source>>]>,
        result_types: Box<[Reference<'source>]>,
        statements: Box<[Statement<'source>]>,
    ) -> Self {
        Self {
            input_registers,
            result_types,
            statements,
        }
    }

    #[inline]
    pub fn input_registers(&self) -> &[Located<TypedRegister<'source>>] {
        &self.input_registers
    }

    #[inline]
    pub fn result_types(&self) -> &[Reference<'source>] {
        &self.result_types
    }

    #[inline]
    pub fn statements(&self) -> &[Statement<'source>] {
        &self.statements
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Directive<'source> {
    /// ```text
    /// .array
    /// ```
    /// Indicates that records that follow the next record after this directive should be merged into a single array record.
    ///
    /// # Example
    ///
    /// ```text
    /// .array ; Merges the following 3 identifier records into one record
    /// .identifier "abc"
    /// .identifier "def"
    /// .identifier "testing"
    /// ```
    Array,
    /// ```text
    /// .format major 0
    /// .format minor 12
    /// ```
    /// Sets the major or minor format version of the module.
    Format(FormatVersionKind, u8),
    /// ```text
    /// .metadata id "MyModule" 1.2.3 ; Specifies the name and version of the module
    /// .metadata id "MyModuleNoVersion"
    /// .meta id "ShortenedVersion"
    /// ```
    /// Specifies information about the module.
    Metadata(Metadata<'source>),
    /// ```text
    /// .identifier "no symbol" ; Referred to by numeric index
    /// .identifier @my_identifier "with symbol" ; Referred to by numeric index or by symbol
    /// .ident "shorter_syntax"
    /// ```
    /// Defines a record containing a reusable identifier string.
    Identifier(Option<Symbol<'source>>, Located<Identifier<'source>>),
    /// ```text
    /// .data 0x68 0x65 0x6C 0x6C 0x6F ; Referred to by numeric index
    /// .data @my_data 0x74 0x65 0x73 0x74 ; Referred to by numeric index or by symbol
    /// ```
    /// Defines a record containing arbitrary data. Used to declare constant values such as string literals.
    Data(Option<Symbol<'source>>, Located<Box<[u8]>>),
    /// ```text
    /// ; Referred to by numeric index
    /// .signature type u32
    /// .sig type u16
    /// .signature function (@parameter_type_1, @parameter_type_2) -> (@return_type_1, @return_type_2)
    /// .signature function () -> ()
    /// .sig func (#3)
    ///
    /// ; Referred to by numeric index or by symbol
    /// .signature @my_type type s64
    /// .signature @my_pointer_type type rawptr #0
    /// .signature @my_function_signature (@my_type, #1) -> (#1)
    /// .sig @shorter_function_signature func (#0, #1) -> (#2)
    /// ```
    /// Defines a record containing a type signature or a function signature. Used to indicate the types of registers, struct
    /// fields, globals, function return values, and function parameters.
    Signature(Option<Symbol<'source>>, Located<Signature<'source>>),
    /// ```text
    /// ; Referred to by numeric index
    /// .code ($a0:@my_argument_type, $a1:@my_other_type) -> (@my_return_type)
    /// nop
    /// $r0:@my_other_type, $r1:@my_other_other_type = call @my_function ($a0, $a1)
    /// $r2:@some_integer = addi $r0, 3
    /// ret
    ///
    /// ; Referred to by numeric index or by symbol
    /// .code @my_code_block ($a0:#12, $a1:#34) -> (#5, #6)
    /// ; Insert instructions here...
    /// ret
    /// ```
    Code(Option<Symbol<'source>>, CodeBlock<'source>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_literal_string_with_escape_sequences_is_valid() {
        assert_eq!(
            "test\nfor\tthings\r\nthat work\\",
            LiteralString::try_from("test\\nfor\\tthings\\r\\nthat work\\\\")
                .unwrap()
                .as_str()
        );
    }
}
