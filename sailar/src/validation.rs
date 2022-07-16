//! Module to perform validation of SAILAR code.
//!
//! Validation ensures that the contents of a SAILAR module are correct, without having to resolve any imports.

use crate::helper::borrow::CowBox;
use crate::index;
use crate::instruction::{self, Instruction};
use crate::record::{self, Record};
use crate::signature;
use std::borrow::Cow;
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

/// A list specifying the kinds of errors that can occur during SAILAR module validation.
///
/// Usually used with the [`Error`] type.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    /// Used when more than one entry point was specified by a metadata record.
    #[error("duplicate entry point #{duplicate}, #{defined} is already defined as the entry point function")]
    DuplicateEntryPoint {
        defined: index::FunctionInstantiation,
        duplicate: index::FunctionInstantiation,
    },
    #[error(transparent)]
    InvalidIndex(#[from] InvalidIndexError),
    /// Used when a cycle is detected in a type signature, resulting in infinite recursion.
    #[error("type signature {0} directly or indirectly refers to itself, resulting in infinite recursion")]
    TypeSignatureCycle(index::TypeSignature),
    #[error("code block {0} must contain at least one instruction")]
    EmptyCodeBlock(index::CodeBlock),
    #[error(transparent)]
    InvalidInstruction(#[from] InvalidInstructionError), //FunctionInputTypesMismatch
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

/// Represents the contents of a SAILAR module.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct ModuleContents<'a> {
    pub module_identifiers: Vec<record::ModuleIdentifier<'a>>,
    pub entry_point: Option<index::FunctionInstantiation>,
    /// The list of all identifier records in the module.
    pub identifiers: Vec<Cow<'a, crate::identifier::Id>>,
    pub type_signatures: Vec<Cow<'a, signature::Type>>,
    pub function_signatures: Vec<Cow<'a, signature::Function>>,
    pub data: Vec<Cow<'a, record::DataArray>>,
    pub code: Vec<CowBox<'a, record::CodeBlock<'a>>>,
    pub function_definitions: Vec<CowBox<'a, record::FunctionDefinition<'a>>>,
}

impl<'a> ModuleContents<'a> {
    /// Indicates whether the module is anonymous.
    ///
    /// Anonymous modules do not have any module identifier, meaning that they cannot be imported by other modules.
    pub fn is_anonymous(&self) -> bool {
        self.module_identifiers.is_empty()
    }
}

/// Represents a validated SAILAR module.
#[derive(Clone, Debug)]
pub struct ValidModule<'a> {
    contents: ModuleContents<'a>,
}

impl<'a> ValidModule<'a> {
    fn validate(mut contents: ModuleContents<'a>, metadata_fields: Vec<record::MetadataField<'a>>) -> Result<Self, Error> {
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

                match signature.as_ref() {
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
                .flat_map(|signature| signature.as_ref().types().iter().copied())
                .try_for_each(|index| {
                    (&check_type_signature_index)(index)?;
                    Result::<_, Error>::Ok(())
                })?;
        }

        struct SignatureComparer<'a, 'b> {
            type_signatures: &'b [Cow<'a, signature::Type>],
            function_signatures: &'b [Cow<'a, signature::Function>],
        }

        impl SignatureComparer<'_, '_> {
            fn are_function_indices_equal(&self, a: index::FunctionSignature, b: index::FunctionSignature) -> bool {
                if a == b {
                    true
                } else {
                    let a_types = self.function_signatures[usize::from(a)].as_ref().types();
                    let b_types = self.function_signatures[usize::from(b)].as_ref().types();

                    if a_types.len() != b_types.len() {
                        false
                    } else {
                        a_types.iter().zip(b_types).all(|(x, y)| self.are_type_indices_equal(*x, *y))
                    }
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
                    self.are_type_signatures_equal(
                        self.type_signatures[usize::from(a)].as_ref(),
                        self.type_signatures[usize::from(b)].as_ref(),
                    )
                }
            }

            //fn check_types_equal(&mut self, a: index::TypeSignature, b: index::TypeSignature)

            //fn check_many_types_equal
        }

        let signature_comparer = SignatureComparer {
            type_signatures: &contents.type_signatures,
            function_signatures: &contents.function_signatures,
        };

        let get_type_signature =
            |index| Result::<_, Error>::Ok(contents.type_signatures[(&check_type_signature_index)(index)?].as_ref());

        let check_code_block_index = get_index_validator::<index::CodeBlock>(contents.code.len());

        let get_code_block = { |index| Result::<_, Error>::Ok(contents.code[(&check_code_block_index)(index)?].as_ref()) };

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

                let next_temporary_register_type =
                    || (&get_type_signature)(block.temporary_types()[current_temporary_count.get()]);

                let mut has_terminator = false;
                let mut instruction_iterator = block.instructions.iter();
                let instruction_index = std::cell::Cell::new(0usize);
                let last_instruction_index = block.instructions.len() - 1;

                while let Some(instruction) = instruction_iterator.next() {
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
                        let index = (&validate_register_index)(register)?;
                        Ok(if index < block.input_count {
                            block.input_types()[index]
                        } else {
                            block.temporary_types()[index - block.input_count]
                        })
                    };

                    let get_register_type = |register| (&get_type_signature)(get_register_type_index(register)?);

                    //let define_temporary_register = |ty: index::TypeSignature| { };

                    let expected_type_for_value = |value: &instruction::Value, expected: &_| -> Result<(), Error> {
                        match (value, expected) {
                            (_, signature::Type::FixedInteger(_)) if expected.is_integer() => Ok(()),
                            (instruction::Value::IndexedRegister(register_index), _)
                                if (&signature_comparer)
                                    .are_type_signatures_equal((&get_register_type)(*register_index)?, expected) =>
                            {
                                Ok(())
                            }
                            _ => invalid_instruction!(ValueTypeMismatchError {
                                value: value.clone(),
                                expected_type: expected.clone(),
                                actual_type: match value {
                                    instruction::Value::Constant(instruction::Constant::Integer(_)) => None,
                                    instruction::Value::IndexedRegister(register) =>
                                        Some((&get_register_type)(*register)?.clone()),
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
                            (&expected_type_for_value)(value, get_type_signature(*expected_type)?)?;
                        }

                        Ok(())
                    };

                    if has_terminator {
                        invalid_instruction!(InvalidInstructionKind::ExpectedTerminatorAsLastInstruction);
                    }

                    match instruction {
                        Instruction::Nop | Instruction::Break => (),
                        Instruction::AddI(arguments) | Instruction::SubI(arguments) => {
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
                        Instruction::Ret(values) => {
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

        for definition in contents.function_definitions.iter() {
            check_function_signature_index(definition.signature)?;

            match definition.body {
                record::FunctionBody::Definition(code_index) => {
                    let entry_block = get_code_block(code_index)?;
                    let signature = contents.function_signatures[usize::from(definition.signature)].as_ref();

                    // TODO: Check that input types match signature parameters.
                    // TODO: Check to see what the eventual return types are (don't compare to entry block's return types)
                }
                record::FunctionBody::Foreign { .. } => (),
            }
        }

        for field in metadata_fields.into_iter() {
            match field {
                record::MetadataField::ModuleIdentifier(identifier) => contents.module_identifiers.push(identifier),
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

        Ok(Self { contents })
    }

    pub fn from_records_fallible<R, E>(records: R) -> Result<Result<Self, Error>, E>
    where
        R: IntoIterator<Item = Result<Record<'a>, E>>,
    {
        let mut contents = ModuleContents::<'a>::default();
        let mut metadata_fields = Vec::new();

        for data in records.into_iter() {
            match data? {
                Record::MetadataField(metadata) => metadata_fields.push(metadata),
                Record::Identifier(identifier) => contents.identifiers.push(identifier),
                Record::TypeSignature(signature) => contents.type_signatures.push(signature),
                Record::FunctionSignature(signature) => contents.function_signatures.push(signature),
                Record::Data(data) => contents.data.push(data),
                Record::CodeBlock(block) => contents.code.push(block),
                bad => todo!("validate {:?}", bad),
            }
        }

        Ok(Self::validate(contents, metadata_fields))
    }

    pub fn from_records<R: IntoIterator<Item = Record<'a>>>(records: R) -> Result<Self, Error> {
        Self::from_records_fallible::<_, std::convert::Infallible>(records.into_iter().map(Ok)).unwrap()
    }

    pub fn contents(&self) -> &ModuleContents<'a> {
        &self.contents
    }

    pub fn into_contents(self) -> ModuleContents<'a> {
        self.contents
    }
}

impl<'a> TryFrom<Vec<Record<'a>>> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: Vec<Record<'a>>) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}

impl<'a> TryFrom<Box<[Record<'a>]>> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: Box<[Record<'a>]>) -> Result<Self, Self::Error> {
        Self::try_from(records.into_vec())
    }
}

impl<'a, const N: usize> TryFrom<[Record<'a>; N]> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: [Record<'a>; N]) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}
