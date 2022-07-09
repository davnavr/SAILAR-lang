//! Module for interacting with SAILAR code blocks.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::instruction;
use sailar::record;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

type Record = record::CodeBlock<'static>;

/// A SAILAR instruction that has not yet been type checked.
type Op = instruction::Instruction;

/// Error type used when the type of a value does not match what was expected
#[derive(Clone, Debug, thiserror::Error)]
#[error("expected type {expected} for value {value}, but got {actual}")]
pub struct ValueTypeMismatchError {
    expected: type_system::Type,
    actual: type_system::Type,
    value: instruction::Value,
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("expected {expected} values, but got {actual}")]
pub struct ValueCountMismatchError {
    expected: usize,
    actual: usize,
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidInstructionKind {
    #[error("expected terminator instruction at end of block")]
    ExpectedTerminator,
    #[error(transparent)]
    ExpectedTypeMismatch(#[from] ValueTypeMismatchError),
    #[error(transparent)]
    ValueCountMismatch(#[from] ValueCountMismatchError),
}

#[derive(Clone, Debug)]
pub struct InstructionLocation {
    index: usize,
    instruction: Op,
}

impl InstructionLocation {
    fn new(index: usize, instruction: Op) -> Self {
        Self { index, instruction }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn instruction(&self) -> &sailar::instruction::Instruction {
        &self.instruction
    }
}

#[derive(Clone)]
pub struct InvalidInstructionErrorInner {
    code_block: Arc<Code>,
    location: Option<InstructionLocation>,
    kind: InvalidInstructionKind,
}

#[derive(Clone, thiserror::Error)]
pub struct InvalidInstructionError(Box<InvalidInstructionErrorInner>);

impl InvalidInstructionError {
    fn new<E, L>(kind: E, location: L, code_block: Arc<Code>) -> Self
    where
        E: Into<InvalidInstructionKind>,
        L: Into<Option<InstructionLocation>>,
    {
        Self(Box::new(InvalidInstructionErrorInner {
            kind: kind.into(),
            location: location.into(),
            code_block,
        }))
    }

    pub fn kind(&self) -> &InvalidInstructionKind {
        &self.0.kind
    }

    pub fn location(&self) -> Option<&InstructionLocation> {
        self.0.location.as_ref()
    }

    pub fn code_block(&self) -> &Arc<Code> {
        &self.0.code_block
    }

    pub fn into_loader_error(self) -> error::LoaderError {
        match self.code_block().module().upgrade() {
            Some(module) => error::InvalidModuleError::new(self, module).into(),
            None => error::DroppedError::new(()).into(),
        }
    }
}

impl From<InvalidInstructionError> for error::LoaderError {
    fn from(error: InvalidInstructionError) -> Self {
        error.into_loader_error()
    }
}

impl Debug for InvalidInstructionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("InvalidInstructionError")
            .field("kind", &self.0.kind)
            .field("location", &self.0.location)
            .finish()
    }
}

impl std::fmt::Display for InvalidInstructionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.location() {
            None => std::fmt::Display::fmt(self.kind(), f),
            Some(location) => write!(
                f,
                "instruction {:?} at index {} is invalid, {}",
                &location.instruction,
                location.index,
                self.kind()
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypedValue {
    value_type: type_system::Type,
    value: instruction::Value,
}

impl TypedValue {
    fn new(value_type: type_system::Type, value: instruction::Value) -> Self {
        Self { value_type, value }
    }

    pub fn value_type(&self) -> &type_system::Type {
        &self.value_type
    }

    pub fn raw_value(&self) -> &instruction::Value {
        &self.value
    }
}

/// Represents a type checked SAILAR instruction.
#[derive(Clone, Debug)]
pub enum Instruction {
    Nop,
    Break,
    Ret(Box<[TypedValue]>),
}

impl Instruction {
    pub fn is_terminator(&self) -> bool {
        matches!(self, Self::Ret(_))
    }
}

pub struct Code {
    record: Box<Record>,
    register_types: type_system::LazySignatureList,
    instructions: lazy_init::Lazy<Result<Vec<Instruction>, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Code {
    pub(crate) fn new(record: Box<Record>, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            record,
            register_types: Default::default(),
            instructions: Default::default(),
            module,
        })
    }

    pub fn record(&self) -> &Record {
        &self.record
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    /// The types of all input registers, results, and temporary registers in that order.
    pub fn register_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types
            .get_or_initialize(&self.module, self.record.register_types().iter().copied())
    }

    pub fn input_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types().map(|types| &types[0..self.record.input_count()])
    }

    pub fn result_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types()
            .map(|types| &types[self.record.input_count()..self.record.input_count() + self.record.result_count()])
    }

    pub fn temporary_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types()
            .map(|types| &types[self.record.input_count() + self.record.result_count()..])
    }

    pub fn get_register_type(&self, index: sailar::index::Register) -> Result<&Arc<type_system::Signature>, error::LoaderError> {
        // Register types will fail to load if module is dropped, so expecting a module here is ok
        let module = module::Module::upgrade_weak(&self.module)?;
        let types = self.register_types()?;
        let i = usize::from(index);
        if types.is_empty() {
            Err(error::InvalidModuleError::new(error::InvalidIndexError::new(index, None), module).into())
        } else {
            let input_types = self.input_types()?;
            if i < input_types.len() {
                Ok(&input_types[i])
            } else {
                let temporary_types = self.temporary_types()?;
                temporary_types.get(i - input_types.len()).ok_or_else(|| {
                    error::InvalidModuleError::new(
                        error::InvalidIndexError::new(index, Some(input_types.len() + temporary_types.len() - 1)),
                        module,
                    )
                    .into()
                })
            }
        }
    }

    fn fail_instruction_validation<E: Into<InvalidInstructionKind>>(
        self: &Arc<Self>,
        kind: E,
        index: Option<usize>,
    ) -> InvalidInstructionError {
        InvalidInstructionError::new(
            kind,
            index.map(|index| InstructionLocation::new(index, self.record.instructions().get(index).unwrap().clone())),
            self.clone(),
        )
    }

    fn validate_body(self: &Arc<Self>) -> Result<Vec<Instruction>, error::LoaderError> {
        struct Validator<'a> {
            _module: Arc<module::Module>,
            index: Option<usize>,
            source: &'a [Op],
            instructions: Vec<Instruction>,
            code: &'a Arc<Code>,
        }

        impl<'a> Validator<'a> {
            fn new(code: &'a Arc<Code>) -> Result<Self, error::LoaderError> {
                let source = code.record.instructions();
                Ok(Self {
                    _module: module::Module::upgrade_weak(&code.module)?,
                    index: None,
                    instructions: Vec::with_capacity(source.len()),
                    source,
                    code,
                })
            }

            fn next_instruction(&mut self) -> Option<&'a Op> {
                let current_index = match self.index {
                    None if self.source.is_empty() => return None,
                    None => 0,
                    Some(index) if index == self.source.len() - 1 => return None,
                    Some(index) => index + 1,
                };

                self.index = Some(current_index);
                self.source.get(current_index)
            }

            fn fail_validation<E: Into<InvalidInstructionKind>>(&self, kind: E) -> error::LoaderError {
                self.code.fail_instruction_validation(kind, self.index).into()
            }

            fn check_value(
                &self,
                value: &instruction::Value,
                expected: type_system::Type,
            ) -> Result<TypedValue, error::LoaderError> {
                match value {
                    instruction::Value::Constant(instruction::Constant::Integer(integer)) => {
                        fn get_integer_bit_width(ty: &type_system::Type) -> Result<type_system::IntegerSize, error::LoaderError> {
                            Ok(match ty {
                                type_system::Type::FixedInteger(ty) => ty.size(),
                                type_system::Type::UAddr
                                | type_system::Type::SAddr
                                | type_system::Type::RawPtr(_)
                                | type_system::Type::FuncPtr(_) => todo!("pointer type size calculation is not yet supported"),
                                type_system::Type::Signature(signature) => {
                                    return signature.signature().and_then(get_integer_bit_width)
                                }
                                type_system::Type::F32 | type_system::Type::F64 => {
                                    todo!("error for expected float but got integer constant")
                                }
                            })
                        }

                        let integer_width = get_integer_bit_width(&expected)?;

                        if integer_width >= integer.bit_size() {
                            Ok(TypedValue::new(expected, value.clone()))
                        } else {
                            todo!("error for constant overflow")
                        }
                    }
                    instruction::Value::IndexedRegister(index) => {
                        let register_type = type_system::Type::Signature(self.code.get_register_type(*index)?.clone());
                        if register_type == expected {
                            Ok(TypedValue::new(register_type, value.clone()))
                        } else {
                            Err(self.fail_validation(ValueTypeMismatchError {
                                value: value.clone(),
                                expected,
                                actual: register_type,
                            }))
                        }
                    }
                }
            }

            fn check_many_values(
                &self,
                values: &[instruction::Value],
                expected: &[Arc<type_system::Signature>],
            ) -> Result<Box<[TypedValue]>, error::LoaderError> {
                if values.len() != expected.len() {
                    return Err(self.fail_validation(ValueCountMismatchError {
                        expected: expected.len(),
                        actual: values.len(),
                    }));
                }

                values
                    .iter()
                    .zip(expected)
                    .map(|(v, e)| self.check_value(v, e.clone().into()))
                    .collect()
            }
        }

        let mut validator = Validator::new(self)?;

        while let Some(op) = validator.next_instruction() {
            validator.instructions.push(match op {
                Op::Nop => Instruction::Nop,
                Op::Break => Instruction::Break,
                Op::Ret(values) => Instruction::Ret(validator.check_many_values(values, self.result_types()?)?),
                bad => todo!("support {:?}", bad),
            });
        }

        match validator.instructions.last() {
            Some(terminator) if terminator.is_terminator() => (),
            _ => return Err(validator.fail_validation(InvalidInstructionKind::ExpectedTerminator)),
        }

        Ok(validator.instructions)
    }

    pub fn instructions<'a>(self: &'a Arc<Self>) -> Result<&'a [Instruction], error::LoaderError> {
        self.instructions
            .get_or_create(|| self.validate_body())
            .as_ref()
            .map(|instructions| instructions.as_slice())
            .map_err(Clone::clone)
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("record", &self.record)
            .field("register_types", &self.register_types)
            .finish()
    }
}
