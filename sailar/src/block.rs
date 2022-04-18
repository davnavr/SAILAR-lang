//! Manipulation of SAILAR code blocks.

use crate::instruction_set::{self, Instruction, TypedValue};
use crate::type_system;
use std::fmt::{Display, Formatter};
use std::iter::IntoIterator;
use std::marker::PhantomData;

#[derive(Clone, Debug, thiserror::Error)]
#[error("expected result at index {index} to be of type {expected:?} but got {actual:?}")]
pub struct InvalidResultTypeError {
    index: usize,
    expected: type_system::Any,
    actual: type_system::Any,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ExpectedTypeErrorKind {
    Expected(type_system::Any),
    ExpectedInteger,
}

#[derive(Clone, Debug)]
pub struct ExpectedTypeError {
    actual: type_system::Any,
    kind: ExpectedTypeErrorKind,
}

impl Display for ExpectedTypeError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("expected ")?;
        match &self.kind {
            ExpectedTypeErrorKind::Expected(expected) => write!(f, "{:?}", expected)?,
            ExpectedTypeErrorKind::ExpectedInteger => f.write_str("integer type")?,
        }
        write!(f, " but got {:?}", self.actual)
    }
}

impl std::error::Error for ExpectedTypeError {}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ValidationError {
    #[error("code blocks must not be empty")]
    EmptyBlock,
    #[error(transparent)]
    InvalidResultType(#[from] InvalidResultTypeError),
    #[error("expected {expected} results but got {actual}")]
    ResultCountMismatch { expected: usize, actual: usize },
    #[error(transparent)]
    ExpectedType(#[from] ExpectedTypeError),
}

pub type ValidationResult<T> = Result<T, Box<ValidationError>>;

#[derive(Debug, Eq, PartialEq)]
pub struct Input {
    index: usize,
    value_type: type_system::Any,
}

impl TypedValue for Input {
    #[inline]
    fn value_type(&self) -> type_system::Any {
        self.value_type.clone()
    }
}

// TODO: If using &'r Temporary, remove the Clone impl.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Temporary<'r> {
    index: usize,
    value_type: type_system::Any,
    owner: PhantomData<&'r ()>,
}

impl TypedValue for Temporary<'_> {
    #[inline]
    fn value_type(&self) -> type_system::Any {
        self.value_type.clone()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Value<'r> {
    Constant(instruction_set::Constant),
    Temporary(Temporary<'r>),
    Input(&'r Input),
}

impl Value<'_> {
    fn into_value(self, input_count: usize) -> instruction_set::Value {
        match self {
            Self::Constant(constant) => instruction_set::Value::Constant(constant),
            Self::Temporary(temporary) => instruction_set::Value::IndexedRegister(temporary.index + input_count),
            Self::Input(input) => instruction_set::Value::IndexedRegister(input.index),
        }
    }
}

crate::enum_case_from_impl!(Value<'_>, Constant, instruction_set::Constant);

impl<'r> From<Temporary<'r>> for Value<'r> {
    #[inline]
    fn from(temporary: Temporary<'r>) -> Self {
        Self::Temporary(temporary)
    }
}

impl<'r> From<&'r Input> for Value<'r> {
    #[inline]
    fn from(input: &'r Input) -> Self {
        Self::Input(input)
    }
}

impl TypedValue for Value<'_> {
    fn value_type(&self) -> type_system::Any {
        match self {
            Self::Constant(constant) => constant.value_type(),
            Self::Temporary(temporary) => temporary.value_type(),
            Self::Input(input) => input.value_type(),
        }
    }
}

macro_rules! integer_to_value_conversion_impl {
    ($integer_type: ty) => {
        impl From<$integer_type> for Value<'_> {
            fn from(value: $integer_type) -> Self {
                Self::Constant(instruction_set::Constant::from(value))
            }
        }
    };
}

integer_to_value_conversion_impl!(u8);
integer_to_value_conversion_impl!(i8);
integer_to_value_conversion_impl!(u16);
integer_to_value_conversion_impl!(i16);
integer_to_value_conversion_impl!(u32);
integer_to_value_conversion_impl!(i32);
integer_to_value_conversion_impl!(u64);
integer_to_value_conversion_impl!(i64);

/// Allows the building of SAILAR code blocks.
#[derive(Debug)]
pub struct Builder<'b> {
    result_types: Box<[type_system::Any]>,
    input_registers: &'b Vec<Input>,
    temporary_register_count: usize,
    instructions: &'b mut Vec<Instruction>,
    value_buffer: &'b mut Vec<instruction_set::Value>,
}

macro_rules! fail {
    ($error: expr) => {
        return Err(Box::new(ValidationError::from($error)));
    };
}

impl<'b> Builder<'b> {
    #[inline]
    pub fn result_types(&self) -> &[type_system::Any] {
        &self.result_types
    }

    #[inline]
    pub fn input_registers(&self) -> &'b [Input] {
        &self.input_registers
    }

    pub fn emit_nop(&mut self) {
        self.instructions.push(Instruction::Nop);
    }

    pub fn emit_break(&mut self) {
        self.instructions.push(Instruction::Break);
    }

    fn define_temporary(&mut self, value_type: type_system::Any) -> Temporary<'b> {
        let index = self.temporary_register_count;
        self.temporary_register_count += 1;
        Temporary {
            index,
            value_type,
            owner: PhantomData,
        }
    }

    

    /// Emit an instruction that adds two integer values and stores it in a temporary register, ignoring any overflows.
    pub fn emit_add<X: Into<Value<'b>>, Y: Into<Value<'b>>>(&mut self, x: X, y: Y) -> ValidationResult<Temporary<'b>> {
        let x_value = x.into();
        let y_value = y.into();
        let x_type = x_value.value_type();
        let y_type = y_value.value_type();

        match x_type {
            type_system::Any::Primitive(type_system::Primitive::Int(_)) => (),
            actual => fail!(ExpectedTypeError {
                actual,
                kind: ExpectedTypeErrorKind::ExpectedInteger,
            }),
        }

        if x_type != y_type {
            fail!(ExpectedTypeError {
                actual: y_type,
                kind: ExpectedTypeErrorKind::Expected(x_type),
            });
        }

        // TODO: Emit the Add.
        Ok(self.define_temporary(x_type.clone()))
    }

    //pub fn emit_add_flagged

    //pub fn emit_add_saturating

    fn finish(self) -> ValidationResult<Block> {
        if self.instructions.is_empty() {
            fail!(ValidationError::EmptyBlock);
        }

        let input_types = self.input_registers().iter().map(|input| input.value_type.clone()).collect();

        Ok(Block {
            result_types: self.result_types,
            input_types,
            instructions: self.instructions.clone().into_boxed_slice(),
        })
    }

    pub fn emit_ret<V: IntoIterator<Item = Value<'b>>>(self, values: V) -> ValidationResult<Block> {
        let return_values = values.into_iter();
        let (minimum_return_count, maximum_return_count) = return_values.size_hint();
        self.value_buffer.clear();
        self.value_buffer
            .reserve(maximum_return_count.unwrap_or(minimum_return_count));

        for (index, (value, expected)) in return_values.zip(self.result_types.iter()).enumerate() {
            let actual = value.value_type();
            if &actual != expected {
                fail!(InvalidResultTypeError {
                    index,
                    expected: expected.clone(),
                    actual,
                });
            }

            self.value_buffer.push(value.into_value(self.input_registers.len()));
        }

        if self.value_buffer.len() != self.result_types.len() {
            fail!(ValidationError::ResultCountMismatch {
                expected: self.result_types.len(),
                actual: self.value_buffer.len(),
            });
        }

        self.instructions
            .push(Instruction::Ret(self.value_buffer.clone().into_boxed_slice()));
        self.finish()
    }
}

#[derive(Debug)]
pub struct BuilderCache {
    instruction_buffer: Vec<Instruction>,
    value_buffer: Vec<instruction_set::Value>,
    input_buffer: Vec<Input>,
}

impl BuilderCache {
    pub fn new() -> Self {
        Self {
            instruction_buffer: Vec::with_capacity(32),
            value_buffer: Vec::default(),
            input_buffer: Vec::default(),
        }
    }

    #[must_use]
    pub fn builder<R: Into<Box<[type_system::Any]>>, I: IntoIterator<Item = type_system::Any>>(
        &mut self,
        result_types: R,
        input_types: I,
    ) -> Builder<'_> {
        self.instruction_buffer.clear();
        self.value_buffer.clear();
        self.input_buffer.clear();

        for (index, input_type) in input_types.into_iter().enumerate() {
            self.input_buffer.push(Input {
                index,
                value_type: input_type,
            });
        }

        Builder {
            result_types: result_types.into(),
            input_registers: &mut self.input_buffer,
            temporary_register_count: 0,
            instructions: &mut self.instruction_buffer,
            value_buffer: &mut self.value_buffer,
        }
    }
}

impl std::default::Default for BuilderCache {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Block {
    result_types: Box<[type_system::Any]>,
    input_types: Box<[type_system::Any]>,
    instructions: Box<[Instruction]>,
}

impl Block {
    #[inline]
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    #[inline]
    pub fn result_types(&self) -> &[type_system::Any] {
        &self.result_types
    }

    #[inline]
    pub fn input_types(&self) -> &[type_system::Any] {
        &self.input_types
    }
}
