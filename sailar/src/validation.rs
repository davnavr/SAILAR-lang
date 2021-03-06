//! Module to perform validation of SAILAR code.
//!
//! Validation ensures that the contents of a SAILAR module are correct, without having to resolve any imports.

use crate::identifier::{Id, Identifier};
use crate::index;
use crate::instruction::{self, Instruction};
use crate::record::{self, Record};
use crate::signature;
use std::borrow::Cow;
use std::collections::hash_map;
use std::fmt::{Display, Formatter};

/// The error type used when an index in a module is not valid.
#[derive(Clone, Debug, thiserror::Error)]
pub struct InvalidIndexError {
    index: usize,
    maximum_index: Option<usize>,
    name: &'static str,
}

impl Display for InvalidIndexError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} index {} is not valid", self.name, self.index)?;
        if let Some(maximum) = self.maximum_index {
            write!(f, ", maximum valid index is {}", maximum)?;
        }
        Ok(())
    }
}

/// The error type used when a value does not have the type that was expected.
#[derive(Clone, Debug, thiserror::Error)]
pub struct ValueTypeMismatchError {
    value: instruction::Value,
    expected_type: signature::Type,
    actual_type: Option<signature::Type>,
}

impl Display for ValueTypeMismatchError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "expected value {} to be of type {}", self.value, self.expected_type)?;
        if let Some(actual_type) = &self.actual_type {
            write!(f, ", but got {}", actual_type)?;
        }
        Ok(())
    }
}

/// The error type used when a function template's parameters or return types do not match it's entry block.
#[derive(Clone, Debug, thiserror::Error)]
pub struct FunctionTypeMismatchError {
    template: index::FunctionTemplate,
    entry_block: index::CodeBlock,
    are_parameters_wrong: bool,
    actual_types: Box<[signature::Type]>,
    expected_types: Box<[signature::Type]>,
}

impl Display for FunctionTypeMismatchError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "function template {} with entry block {} has the ",
            self.template, self.entry_block
        )?;
        f.write_str(if self.are_parameters_wrong { "parameter" } else { "return" })?;
        write!(f, " types {}, but the ", signature::DisplayTypes::from(&self.actual_types))?;
        f.write_str(if self.are_parameters_wrong {
            "entry block had input types of "
        } else {
            "expected return types were inferred to be "
        })?;
        Display::fmt(&signature::DisplayTypes::from(&self.expected_types), f)
    }
}

/// A list specifying the different ways in which an instruction is considered invalid.
///
/// Used with the [`InvalidInstructionError`] type.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidInstructionKind {
    #[error("expected terminator instruction at end of block")]
    ExpectedTerminator,
    #[error("no instructions should come after the first terminator instruction")]
    ExpectedTerminatorAsLastInstruction,
    #[error(transparent)]
    InvalidIndex(#[from] InvalidIndexError),
    #[error("attempted to define more than {count} temporary registers")]
    ExtraneousTemporary { count: usize },
    #[error("expected result register (register {register}) to be an integer type, but got {actual_type}")]
    ExpectedIntegerResult {
        register: index::Register,
        actual_type: signature::Type,
    },
    #[error(transparent)]
    ExpectedTypeForValue(#[from] ValueTypeMismatchError),
    #[error("expected {expected} values, but got {actual}")]
    ValueCountMismatch { expected: usize, actual: usize },
}

/// The error type used when a SAILAR instruction is invalid.
///
/// Used with the [`ErrorKind`] type to indicate that a SAILAR code block is not valid.
#[derive(Clone, Debug, thiserror::Error)]
pub struct InvalidInstructionError {
    instruction_index: usize,
    instruction: Instruction,
    code_block: index::CodeBlock,
    kind: InvalidInstructionKind,
}

impl Display for InvalidInstructionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "instruction \"{:?}\" at index {}, in code block {} is invalid: {}",
            self.instruction, self.instruction_index, self.code_block, self.kind
        )
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum SymbolIndex {
    FunctionTemplate(index::FunctionTemplate),
}

crate::enum_case_from_impl!(SymbolIndex, FunctionTemplate, index::FunctionTemplate);

impl Display for SymbolIndex {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::FunctionTemplate(index) => write!(f, "{} {}", <index::FunctionTemplate as index::Index>::name(), index),
        }
    }
}

/// A list specifying the kinds of errors that can occur during SAILAR module validation.
///
/// Usually used with the [`Error`] type.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    /// Used when more than one entry point was specified by a metadata record.
    #[error("duplicate entry point #{duplicate}, #{defined} is already defined as the entry point function")]
    DuplicateEntryPoint {
        defined: index::Function,
        duplicate: index::Function,
    },
    #[error(transparent)]
    InvalidIndex(#[from] InvalidIndexError),
    /// Used when a cycle is detected in a type signature, resulting in infinite recursion.
    #[error("type signature {0} directly or indirectly refers to itself, resulting in infinite recursion")]
    TypeSignatureCycle(index::TypeSignature),
    #[error("code block {0} must contain at least one instruction")]
    EmptyCodeBlock(index::CodeBlock),
    #[error(transparent)]
    InvalidInstruction(#[from] InvalidInstructionError),
    #[error(transparent)]
    FunctionTypeMismatch(#[from] FunctionTypeMismatchError),
    #[error("{duplicate} has symbol {symbol:?}, but that symbol already corresponds to {existing}")]
    DuplicateSymbol {
        symbol: Identifier,
        existing: SymbolIndex,
        duplicate: SymbolIndex,
    },
}

/// Represents an error that occured during validation of a SAILAR module.
#[derive(Clone, Debug, thiserror::Error)]
#[error(transparent)]
#[repr(transparent)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub fn from_kind<E: Into<ErrorKind>>(kind: E) -> Self {
        Self(Box::new(kind.into()))
    }
}

impl<E: Into<ErrorKind>> From<E> for Error {
    fn from(error: E) -> Self {
        Self::from_kind(error)
    }
}

/// Indicates which definitions in a module are exported.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct Exports {
    pub function_templates: Vec<index::FunctionTemplate>,
}

pub type ModuleIdentifierSet<'data> = rustc_hash::FxHashSet<record::ModuleIdentifier<'data>>;

/// Represents the contents of a SAILAR module.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct ModuleContents<'data> {
    pub module_identifiers: ModuleIdentifierSet<'data>,
    pub entry_point: Option<index::Function>,
    /// The list of all identifier records in the module.
    pub identifiers: Vec<Cow<'data, crate::identifier::Id>>,
    pub type_signatures: Vec<signature::Type>,
    pub function_signatures: Vec<signature::Function>,
    pub data: Vec<Cow<'data, [u8]>>,
    pub code: Vec<record::CodeBlock<'data>>,
    pub function_templates: Vec<record::FunctionTemplate<'data>>,
    pub functions: Vec<record::Function<'data>>,
}

impl<'data> ModuleContents<'data> {
    /// Indicates whether the module is anonymous.
    ///
    /// Anonymous modules do not have any module identifier, meaning that they cannot be imported by other modules.
    pub fn is_anonymous(&self) -> bool {
        self.module_identifiers.is_empty()
    }
}

/// Represents a validated SAILAR module.
#[derive(Clone, Default)]
pub struct ValidModule<'data> {
    contents: ModuleContents<'data>,
    exports: Exports,
}

impl<'data> ValidModule<'data> {
    /// Creates a valid module with the specified `contents`, without actually performing any validation.
    ///
    /// Passing a module that is not valid may result in panics later.
    pub fn from_contents_without_performing_validation_at_all(contents: ModuleContents<'data>, exports: Exports) -> Self {
        Self { contents, exports }
    }

    fn validate(mut contents: ModuleContents<'data>, metadata_fields: Vec<record::MetadataField<'data>>) -> Result<Self, Error> {
        fn get_index_validator<I: index::Index>(length: usize) -> impl Fn(I) -> Result<usize, Error> {
            move |index: I| {
                let index = index.into();
                if index < length {
                    Ok(index)
                } else {
                    Err(InvalidIndexError {
                        index,
                        maximum_index: if length == 0 { None } else { Some(length - 1) },
                        name: I::name(),
                    })?
                }
            }
        }

        let check_type_signature_index = get_index_validator(contents.type_signatures.len());
        let check_function_signature_index = get_index_validator(contents.function_signatures.len());

        {
            /// The values are the types that directly refer to the key.
            type TypeReferenceLookup = rustc_hash::FxHashMap<index::TypeSignature, rustc_hash::FxHashSet<index::TypeSignature>>;

            let mut type_reference_lookup: TypeReferenceLookup = Default::default();

            for (i, signature) in contents.type_signatures.iter().enumerate() {
                let current_index = index::TypeSignature::from(i);

                match signature {
                    signature::Type::FixedInteger(_)
                    | signature::Type::F32
                    | signature::Type::F64
                    | signature::Type::SAddr
                    | signature::Type::UAddr
                    | signature::Type::RawPtr(None) => (),
                    signature::Type::RawPtr(Some(pointee)) => {
                        check_type_signature_index(*pointee)?;
                        type_reference_lookup.entry(*pointee).or_default().insert(current_index);
                    }
                    signature::Type::FuncPtr(signature) => {
                        check_function_signature_index(*signature)?;

                        // Recursive function signatures should be prevented here
                        contents.function_signatures[usize::from(*signature)]
                            .types()
                            .iter()
                            .copied()
                            .for_each(|type_signature| {
                                type_reference_lookup.entry(type_signature).or_default().insert(current_index);
                            });
                    }
                }
            }

            let mut type_referer_buffer = Vec::<index::TypeSignature>::with_capacity(type_reference_lookup.len());
            let mut type_referent_lookup: rustc_hash::FxHashSet<index::TypeSignature> = Default::default();
            for (referent, referers) in type_reference_lookup.iter() {
                type_referent_lookup.clear();
                type_referer_buffer.clear();
                type_referer_buffer.extend(referers);

                while let Some(referer) = type_referer_buffer.pop() {
                    if referer == *referent {
                        return Err(ErrorKind::TypeSignatureCycle(referer))?;
                    } else if type_referent_lookup.insert(referer) {
                        if let Some(indirect_referers) = type_reference_lookup.get(&referer) {
                            type_referer_buffer.extend(indirect_referers);
                        }
                    }
                }
            }

            contents
                .function_signatures
                .iter()
                .flat_map(|signature| signature.types().iter().copied())
                .try_for_each(|index| {
                    check_type_signature_index(index)?;
                    Result::<_, Error>::Ok(())
                })?;
        }

        struct SignatureComparer<'a> {
            type_signatures: &'a [signature::Type],
            function_signatures: &'a [signature::Function],
        }

        impl SignatureComparer<'_> {
            fn are_type_index_lists_equal(&self, a: &[index::TypeSignature], b: &[index::TypeSignature]) -> bool {
                a.len() == a.len() && a.iter().zip(b).all(|(x, y)| self.are_type_indices_equal(*x, *y))
            }

            fn are_function_indices_equal(&self, a: index::FunctionSignature, b: index::FunctionSignature) -> bool {
                if a == b {
                    true
                } else {
                    self.are_type_index_lists_equal(
                        self.function_signatures[usize::from(a)].types(),
                        self.function_signatures[usize::from(b)].types(),
                    )
                }
            }

            fn are_type_signatures_equal(&self, x: &signature::Type, y: &signature::Type) -> bool {
                match (x, y) {
                    (signature::Type::RawPtr(Some(c)), signature::Type::RawPtr(Some(d))) => self.are_type_indices_equal(*c, *d),
                    (signature::Type::FuncPtr(c), signature::Type::FuncPtr(d)) => self.are_function_indices_equal(*c, *d),
                    (signature::Type::FixedInteger(c), signature::Type::FixedInteger(d)) => c == d,
                    (signature::Type::F32, signature::Type::F32)
                    | (signature::Type::F64, signature::Type::F64)
                    | (signature::Type::UAddr, signature::Type::UAddr)
                    | (signature::Type::SAddr, signature::Type::SAddr)
                    | (signature::Type::RawPtr(Some(_)), signature::Type::RawPtr(None))
                    | (signature::Type::RawPtr(None), signature::Type::RawPtr(Some(_)))
                    | (signature::Type::RawPtr(None), signature::Type::RawPtr(None)) => true,
                    _ => false,
                }
            }

            fn are_type_indices_equal(&self, a: index::TypeSignature, b: index::TypeSignature) -> bool {
                if a == b {
                    true
                } else {
                    self.are_type_signatures_equal(&self.type_signatures[usize::from(a)], &self.type_signatures[usize::from(b)])
                }
            }
        }

        let signature_comparer = SignatureComparer {
            type_signatures: &contents.type_signatures,
            function_signatures: &contents.function_signatures,
        };

        let get_type_signature = |index| Result::<_, Error>::Ok(&contents.type_signatures[check_type_signature_index(index)?]);

        let get_type_signature_list_owned = |indices: &[index::TypeSignature]| {
            indices
                .iter()
                .copied()
                .map(|index| get_type_signature(index).cloned())
                .collect::<Result<Box<[_]>, Error>>()
        };

        let check_code_block_index = get_index_validator::<index::CodeBlock>(contents.code.len());

        let get_code_block = { |index| Result::<_, Error>::Ok(&contents.code[check_code_block_index(index)?]) };

        // TODO: Have a hashmap of all of the blocks that a block potentially branches to, as well as the eventual return type.
        {
            //let mut branch_targets = Vec::<(index::CodeBlock, Box<[signature::Type]>)>::new();
            for (block_index, block) in contents.code.iter().enumerate() {
                let block_index = index::CodeBlock::from(block_index);

                if block.instructions.is_empty() {
                    return Err(ErrorKind::EmptyCodeBlock(block_index))?;
                }

                let total_register_count = block.register_count();
                let last_register_index = if total_register_count > 0 {
                    Some(total_register_count - 1)
                } else {
                    None
                };

                let current_temporary_count = std::cell::Cell::new(0usize);
                let increment_temporary_count = || current_temporary_count.set(current_temporary_count.get() + 1);

                let next_temporary_register_index = || index::Register::from(block.input_count + current_temporary_count.get());

                let next_temporary_register_type = || get_type_signature(block.temporary_types()[current_temporary_count.get()]);

                let mut has_terminator = false;
                let instruction_iterator = block.instructions.iter();
                let instruction_index = std::cell::Cell::new(0usize);
                let last_instruction_index = block.instructions.len() - 1;

                for instruction in instruction_iterator {
                    macro_rules! invalid_instruction {
                        ($kind:expr) => {
                            return Err(InvalidInstructionError {
                                instruction_index: instruction_index.get(),
                                code_block: block_index,
                                instruction: instruction.clone(),
                                kind: $kind.into(),
                            })?
                        };
                    }

                    let validate_register_index = |register: index::Register| -> Result<usize, Error> {
                        let index = usize::from(register);
                        match last_register_index {
                            Some(last) if index <= last => Ok(index),
                            None | Some(_) => invalid_instruction!(InvalidIndexError {
                                index,
                                maximum_index: last_register_index,
                                name: <index::Register as index::Index>::name(),
                            }),
                        }
                    };

                    let get_register_type_index = |register| -> Result<_, Error> {
                        let index = validate_register_index(register)?;
                        Ok(if index < block.input_count {
                            block.input_types()[index]
                        } else {
                            block.temporary_types()[index - block.input_count]
                        })
                    };

                    let get_register_type = |register| get_type_signature(get_register_type_index(register)?);

                    //let define_temporary_register = |ty: index::TypeSignature| { };

                    let expected_type_for_value = |value: &instruction::Value, expected: &_| -> Result<(), Error> {
                        match (value, expected) {
                            (_, signature::Type::FixedInteger(_)) if expected.is_integer() => Ok(()),
                            (instruction::Value::IndexedRegister(register_index), _)
                                if (&signature_comparer)
                                    .are_type_signatures_equal(get_register_type(*register_index)?, expected) =>
                            {
                                Ok(())
                            }
                            _ => invalid_instruction!(ValueTypeMismatchError {
                                value: value.clone(),
                                expected_type: expected.clone(),
                                actual_type: match value {
                                    instruction::Value::Constant(instruction::Constant::Integer(_)) => None,
                                    instruction::Value::IndexedRegister(register) => Some(get_register_type(*register)?.clone()),
                                },
                            }),
                        }
                    };

                    let expected_types_for_values = |values: &[_], expected: &[_]| -> Result<(), Error> {
                        if values.len() != expected.len() {
                            invalid_instruction!(InvalidInstructionKind::ValueCountMismatch {
                                expected: expected.len(),
                                actual: values.len()
                            });
                        }

                        for (value, expected_type) in values.iter().zip(expected) {
                            expected_type_for_value(value, get_type_signature(*expected_type)?)?;
                        }

                        Ok(())
                    };

                    if has_terminator {
                        invalid_instruction!(InvalidInstructionKind::ExpectedTerminatorAsLastInstruction);
                    }

                    match instruction {
                        Instruction::Nop | Instruction::Break => (),
                        Instruction::IAdd(arguments) | Instruction::ISub(arguments) => {
                            let operand_type = next_temporary_register_type()?;

                            if !operand_type.is_integer() {
                                invalid_instruction!(InvalidInstructionKind::ExpectedIntegerResult {
                                    register: next_temporary_register_index(),
                                    actual_type: operand_type.clone()
                                });
                            }

                            expected_type_for_value(arguments.x_value(), operand_type)?;
                            expected_type_for_value(arguments.y_value(), operand_type)?;
                            increment_temporary_count(); // The result of the operation is the operand_type

                            match arguments.overflow_behavior() {
                                instruction::OverflowBehavior::Flag => {
                                    // TODO: Simply check if the next temporary register type is a boolean
                                    todo!("Define another register for arithmetic overflow")
                                }
                                instruction::OverflowBehavior::Ignore | instruction::OverflowBehavior::Saturate => (),
                            }
                        }
                        Instruction::Return(values) => {
                            expected_types_for_values(values.as_ref(), block.result_types())?;
                            has_terminator = true;
                        }
                        bad => todo!("validate {:?}", bad),
                    }

                    if instruction_index.get() < last_instruction_index {
                        instruction_index.set(instruction_index.get() + 1);
                    } else if !has_terminator {
                        invalid_instruction!(InvalidInstructionKind::ExpectedTerminator);
                    }
                }
            }
        }

        #[derive(Default)]
        struct SymbolLookup<'a>(rustc_hash::FxHashMap<&'a Id, SymbolIndex>);

        impl<'a> SymbolLookup<'a> {
            fn try_insert(&mut self, symbol: &'a Id, index: SymbolIndex) -> Result<(), Error> {
                match self.0.entry(symbol) {
                    hash_map::Entry::Occupied(occupied) => {
                        return Err(ErrorKind::DuplicateSymbol {
                            symbol: Identifier::from_id(occupied.key()),
                            existing: *occupied.get(),
                            duplicate: index,
                        })?
                    }
                    hash_map::Entry::Vacant(vacant) => {
                        vacant.insert(index);
                        Ok(())
                    }
                }
            }
        }

        let mut symbol_lookup = SymbolLookup::default();
        let mut exports = Exports::default();

        for (index, template) in contents.function_templates.iter().enumerate() {
            let current_index = index::FunctionTemplate::from(index);
            check_function_signature_index(template.signature)?;

            if let Some(symbol) = template.export.symbol() {
                symbol_lookup.try_insert(symbol, current_index.into())?;
            }

            let entry_block = get_code_block(template.entry_block)?;
            let signature = &contents.function_signatures[usize::from(template.signature)];

            if !signature_comparer.are_type_index_lists_equal(entry_block.input_types(), signature.parameter_types()) {
                return Err(FunctionTypeMismatchError {
                    template: current_index,
                    entry_block: template.entry_block,
                    are_parameters_wrong: true,
                    actual_types: get_type_signature_list_owned(signature.parameter_types())?,
                    expected_types: get_type_signature_list_owned(entry_block.input_types())?,
                })?;
            }

            // TODO: Check to see what the eventual return types are (don't compare to entry block's return types)

            if template.export.kind() == record::ExportKind::Export {
                exports.function_templates.push(current_index);
            }
        }

        let check_function_template_index = get_index_validator(contents.function_templates.len());

        for instantiation in contents.functions.iter() {
            check_function_template_index(instantiation.template)?;
        }

        for field in metadata_fields.into_iter() {
            match field {
                record::MetadataField::ModuleIdentifier(identifier) => {
                    contents.module_identifiers.insert(identifier);
                }
                record::MetadataField::EntryPoint(entry_point) => {
                    if let Some(defined) = contents.entry_point {
                        return Err(ErrorKind::DuplicateEntryPoint {
                            defined,
                            duplicate: entry_point,
                        })?;
                    }
                    // else if entry point OOB

                    contents.entry_point = Some(entry_point);
                }
            }
        }

        Ok(Self { contents, exports })
    }

    pub fn from_records_fallible<R, E>(records: R) -> Result<Result<Self, Error>, E>
    where
        R: IntoIterator<Item = Result<Record<'data>, E>>,
    {
        let mut contents = ModuleContents::<'data>::default();
        let mut metadata_fields = Vec::new();

        for data in records.into_iter() {
            match data? {
                Record::MetadataField(metadata) => metadata_fields.push(metadata),
                Record::Identifier(identifier) => contents.identifiers.push(identifier),
                Record::TypeSignature(signature) => contents.type_signatures.push(signature),
                Record::FunctionSignature(signature) => contents.function_signatures.push(signature),
                Record::Data(data) => contents.data.push(data),
                Record::CodeBlock(block) => contents.code.push(block),
                Record::FunctionTemplate(template) => contents.function_templates.push(template),
                Record::Function(function) => contents.functions.push(function),
            }
        }

        Ok(Self::validate(contents, metadata_fields))
    }

    pub fn from_records<R: IntoIterator<Item = Record<'data>>>(records: R) -> Result<Self, Error> {
        Self::from_records_fallible::<_, std::convert::Infallible>(records.into_iter().map(Ok)).unwrap()
    }

    pub fn from_builder(builder: crate::builder::Builder<'static>) -> Result<Self, Error> {
        Self::from_records(builder.into_records())
    }

    pub fn contents(&self) -> &ModuleContents<'data> {
        &self.contents
    }

    pub fn exports(&self) -> &Exports {
        &self.exports
    }

    pub fn into_contents(self) -> ModuleContents<'data> {
        self.contents
    }
}

impl<'data> TryFrom<Vec<Record<'data>>> for ValidModule<'data> {
    type Error = Error;

    fn try_from(records: Vec<Record<'data>>) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}

impl<'data> TryFrom<Box<[Record<'data>]>> for ValidModule<'data> {
    type Error = Error;

    fn try_from(records: Box<[Record<'data>]>) -> Result<Self, Self::Error> {
        Self::try_from(records.into_vec())
    }
}

impl<'data, const N: usize> TryFrom<[Record<'data>; N]> for ValidModule<'data> {
    type Error = Error;

    fn try_from(records: [Record<'data>; N]) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}

impl<'data> std::fmt::Debug for ValidModule<'data> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.contents, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::validation::ValidModule;

    #[test]
    fn empty_module_is_always_valid() {
        ValidModule::from_records(std::iter::empty()).unwrap();
    }
}
