//! Module for interacting with compilation targets.
//!
//! To avoid errors when constructing target information, ensure that LLVM target information has been initialized beforehand,
//! such as by calling [`inkwell::targets::Target::initialize_all`].

use std::borrow::Cow;

pub use inkwell::targets::{
    Target as LlvmTarget, TargetData as LlvmTargetData, TargetMachine as LlvmTargetMachine, TargetTriple as LlvmTargetTriple,
};

/// Represents errors that can occur when constructing target information.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("invalid target triple: {0}")]
    InvalidTargetTriple(String),
    #[error("could not construct target machine for triple {0:?}")]
    TargetMachineConstructionFailed(inkwell::targets::TargetTriple),
}

/// The error type used when constructing target information fails.
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
#[repr(transparent)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub fn new<E: Into<ErrorKind>>(kind: E) -> Self {
        Self(Box::new(kind.into()))
    }
}

impl<E: Into<ErrorKind>> From<E> for Error {
    fn from(kind: E) -> Self {
        Self::new(kind)
    }
}

/// Encapsulates LLVM target information.
#[derive(Debug)]
pub struct Target {
    triple: LlvmTargetTriple,
    machine: LlvmTargetMachine,
    data: LlvmTargetData,
}

impl Target {
    pub fn triple(&self) -> &LlvmTargetTriple {
        &self.triple
    }

    pub fn machine(&self) -> &LlvmTargetMachine {
        &self.machine
    }

    pub fn data(&self) -> &LlvmTargetData {
        &self.data
    }

    pub fn from_machine(machine: LlvmTargetMachine) -> Self {
        Self {
            triple: machine.get_triple(),
            data: machine.get_target_data(),
            machine,
        }
    }

    pub fn new(
        triple: LlvmTargetTriple,
        cpu: &str,
        features: &str,
        optimization_level: inkwell::OptimizationLevel,
        reloc_mode: inkwell::targets::RelocMode,
        code_model: inkwell::targets::CodeModel,
    ) -> Result<Self, Error> {
        let target_machine = LlvmTarget::create_target_machine(
            &LlvmTarget::from_triple(&triple).map_err(|msg| ErrorKind::InvalidTargetTriple(msg.to_string()))?,
            &triple,
            cpu,
            features,
            optimization_level,
            reloc_mode,
            code_model,
        );

        if let Some(machine) = target_machine {
            Ok(Self {
                triple,
                data: machine.get_target_data(),
                machine,
            })
        } else {
            Err(ErrorKind::TargetMachineConstructionFailed(triple).into())
        }
    }

    // TODO: Figure out if CPU name and features correspond to LLVMGtTargetName/LLVMGetTargetDescription

    pub fn host_machine(optimization_level: inkwell::OptimizationLevel) -> Result<Self, Error> {
        Self::new(
            LlvmTargetMachine::get_default_triple(),
            &LlvmTargetMachine::get_host_cpu_name().to_string_lossy(),
            &LlvmTargetMachine::get_host_cpu_features().to_string_lossy(),
            optimization_level,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
    }
}

impl From<LlvmTargetMachine> for Target {
    fn from(machine: LlvmTargetMachine) -> Self {
        Self::from_machine(machine)
    }
}

/// Indicates the size of an integer type.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct IntegerSize(std::num::NonZeroU8);

impl IntegerSize {
    /// Creates an integer size of the specified byte size.
    ///
    /// # Safety
    ///
    /// The size must not be zero.
    #[must_use]
    pub const unsafe fn from_byte_size_unchecked(size: u8) -> Self {
        Self(std::num::NonZeroU8::new_unchecked(size))
    }

    /// Creates an integer size corresponding to the size of the type.
    ///
    /// # Safety
    ///
    /// The size of the type must not be zero.
    ///
    /// # Panics
    ///
    /// Panics if the size of the type is greater than [`u32::MAX`].
    #[must_use]
    pub const unsafe fn from_type_size_unchecked<T>() -> Self {
        let size = std::mem::size_of::<T>();
        assert!(size < u32::MAX as usize);
        Self::from_byte_size_unchecked(size as u8)
    }

    pub const BYTES_2: Self = unsafe { Self::from_byte_size_unchecked(2) };

    pub const BYTES_4: Self = unsafe { Self::from_byte_size_unchecked(4) };

    pub const BYTES_8: Self = unsafe { Self::from_byte_size_unchecked(8) };

    /// Gets the size of the integer, in bytes.
    pub const fn byte_size(self) -> std::num::NonZeroU8 {
        self.0
    }

    /// Gets the size of the integer, in bits.
    pub const fn bit_size(self) -> std::num::NonZeroU16 {
        unsafe {
            // Safety: Size is guaranteed to never be zero, and an overflow won't occur.
            std::num::NonZeroU16::new_unchecked(self.byte_size().get() as u16 * 8u16)
        }
    }

    pub fn get_llvm_integer_type(self, context: &inkwell::context::Context) -> inkwell::types::IntType {
        context.custom_width_int_type(u32::from(self.bit_size().get()))
    }
}

/// Describes the C integer sizes for a target platform.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct CDataModel {
    pub short_size: IntegerSize,
    pub char_size: IntegerSize,
    pub int_size: IntegerSize,
    pub long_size: IntegerSize,
    pub long_long_size: IntegerSize,
}

impl CDataModel {
    /// Gets the C integer sizes of the host platform.
    pub const HOST: Self = unsafe {
        use std::os::raw;

        // Safety: All host integer sizes are not zero.
        Self {
            short_size: IntegerSize::from_type_size_unchecked::<raw::c_short>(),
            char_size: IntegerSize::from_type_size_unchecked::<raw::c_char>(),
            int_size: IntegerSize::from_type_size_unchecked::<raw::c_int>(),
            long_size: IntegerSize::from_type_size_unchecked::<raw::c_long>(),
            long_long_size: IntegerSize::from_type_size_unchecked::<raw::c_longlong>(),
        }
    };
}

impl<'a> From<&'a CDataModel> for Cow<'a, CDataModel> {
    fn from(data_model: &'a CDataModel) -> Self {
        Self::Borrowed(data_model)
    }
}

impl From<CDataModel> for Cow<'_, CDataModel> {
    fn from(data_model: CDataModel) -> Self {
        Self::Owned(data_model)
    }
}

/// Encapsulates information about a target platform.
#[derive(Debug)]
pub struct Platform<'info> {
    target: Target,
    c_data_model: Cow<'info, CDataModel>,
}

impl<'info> Platform<'info> {
    pub fn new<D: Into<Cow<'info, CDataModel>>>(target: Target, c_data_model: D) -> Self {
        Self {
            target,
            c_data_model: c_data_model.into(),
        }
    }

    pub fn target(&self) -> &Target {
        &self.target
    }

    pub fn c_data_model(&self) -> &CDataModel {
        &self.c_data_model
    }
}

impl Platform<'static> {
    pub fn host(optimization_level: inkwell::OptimizationLevel) -> Result<Self, Error> {
        Ok(Self::new(Target::host_machine(optimization_level)?, &CDataModel::HOST))
    }
}
