use crate::buffers;
use crate::format::{self, flags, instruction_set, numeric, type_system};
use sha2::{Digest, Sha256};
use std::io::Read;

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("the file magic indicates that it is not a valid binary module")]
    InvalidModuleMagic,
    #[error("{0:#02X} is not a valid integer size value")]
    InvalidIntegerSize(u8),
    #[error("The format version {}.{} is not supported", .0.major, .0.minor)]
    UnsupportedFormatVersion(format::FormatVersion),
    #[error("{0} is not a valid data vector count")]
    InvalidDataVectorCount(numeric::UInteger),
    #[error("expected to read {expected} bytes but got {actual}")]
    InvalidByteLength { expected: usize, actual: usize },
    #[error("{0} is not a valid number of fields for the module header")]
    InvalidHeaderFieldCount(numeric::UInteger),
    #[error(transparent)]
    InvalidIdentifierCharacter(#[from] std::str::Utf8Error),
    #[error("identifiers must not be empty")]
    EmptyIdentifier,
    #[error("{0:#02X} is not a valid namespace flags combination")]
    InvalidNamespaceFlags(u8),
    #[error(transparent)]
    InvalidTypeSignature(#[from] type_system::InvalidTagError),
    #[error("invalid primitive type, {0}")]
    InvalidPrimitiveType(#[from] type_system::TryFromTagError),
    #[error(transparent)]
    InvalidFixedIntegerType(#[from] type_system::InvalidFixedIntegerTypeError),
    #[error("{0:#02X} is not a valid combination of code block flags")]
    InvalidCodeBlockFlags(u8),
    #[error(transparent)]
    InvalidOpcode(#[from] instruction_set::InvalidOpcodeError),
    #[error("duplicate switch branch for value {0:?}")]
    DuplicateSwitchBranch(instruction_set::IntegerConstant),
    #[error("{0:#02X} is not a valid arithmetic flags combination")]
    InvalidArithmeticFlags(u8),
    #[error("{0:#02X} is not a valid numeric type")]
    InvalidNumericType(u8),
    #[error("{0:#02X} is not a valid overflow behavior")]
    InvalidOverflowBehavior(u8),
    #[error("{0:#02X} is not a valid rotation direction")]
    InvalidRotationDirection(u8),
    #[error("{0:#02X} is not a valid comparison kind")]
    InvalidComparisonKind(u8),
    #[error("{0:#02X} is not a valid memory initialization flags combination")]
    InvalidMemoryInitializationFlags(u8),
    #[error("{0:#02X} is not a valid struct flags combination")]
    InvalidStructFlags(u8),
    #[error("{0:#02X} is not a valid field flags combination")]
    InvalidFieldFlags(u8),
    #[error("{0:#02X} is not a valid function flags combination")]
    InvalidFunctionFlags(u8),
    #[error("{0:#02X} is not a valid struct layout")]
    InvalidStructLayoutFlags(u8),
    #[error("{0:#02X} is not a valid module hash algorithm")]
    InvalidModuleHashAlgorithm(u8),
    #[error(transparent)]
    InputOutputError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Box<Error>>;

struct Input<'a, R> {
    source: R,
    integer_size: numeric::IntegerSize,
    buffer_pool: &'a buffers::BufferPool,
    hasher: &'a mut Sha256,
}

impl<R: Read> Input<'_, R> {
    fn change_input<I: Read>(&mut self, input: I) -> Input<'_, I> {
        Input {
            source: input,
            integer_size: self.integer_size,
            buffer_pool: self.buffer_pool,
            hasher: self.hasher,
        }
    }

    fn fill_buffer(&mut self, buffer: &mut [u8]) -> Result<()> {
        let count = self.source.read(buffer).map_err(Error::InputOutputError)?;

        // TODO: update hash with buffer.

        if count == buffer.len() {
            Ok(())
        } else {
            Err(Box::new(Error::InvalidByteLength {
                expected: buffer.len(),
                actual: count,
            }))
        }
    }

    fn fixed_bytes<const L: usize>(&mut self) -> Result<[u8; L]> {
        let mut buffer = [0u8; L];
        self.fill_buffer(&mut buffer)?;
        Ok(buffer)
    }

    fn byte(&mut self) -> Result<u8> {
        Ok(self.fixed_bytes::<1>()?[0])
    }

    fn many_bytes(&mut self, length: usize) -> Result<Vec<u8>> {
        let mut buffer = vec![0u8; length];
        self.fill_buffer(buffer.as_mut_slice())?;
        Ok(buffer)
    }

    fn unsigned_integer(&mut self) -> Result<numeric::UInteger> {
        match self.integer_size {
            numeric::IntegerSize::I1 => self.byte().map(numeric::UInteger::from),
            numeric::IntegerSize::I2 => self
                .fixed_bytes::<2>()
                .map(|buffer| numeric::UInteger::from(u16::from_le_bytes(buffer))),
            numeric::IntegerSize::I4 => self
                .fixed_bytes::<4>()
                .map(|buffer| numeric::UInteger(u32::from_le_bytes(buffer))),
        }
    }

    fn unsigned_length(&mut self) -> Result<usize> {
        self.unsigned_integer()
            .map(|value| usize::try_from(value).unwrap())
    }

    fn unsigned_index<I: From<numeric::UInteger>>(&mut self) -> Result<I> {
        self.unsigned_integer().map(I::from)
    }

    fn identifier(&mut self) -> Result<format::Identifier> {
        let length = self.unsigned_length()?;
        let mut buffer = self.buffer_pool.rent_with_length(length);
        self.fill_buffer(&mut buffer)?;
        let string = std::str::from_utf8(&buffer).map_err(Error::InvalidIdentifierCharacter)?;
        Ok(format::Identifier::try_from(string).map_err(|_| Error::EmptyIdentifier)?)
    }

    fn length_encoded_vector<T, P: FnMut(&mut Self) -> Result<T>>(
        &mut self,
        mut parser: P,
    ) -> Result<format::LenVec<T>> {
        let length = self.unsigned_length()?;
        let mut buffer = Vec::<T>::with_capacity(length);

        for _ in 0..length {
            let item = parser(self)?;
            buffer.push(item);
        }

        Ok(format::LenVec(buffer))
    }

    fn length_encoded_indices<I: From<numeric::UInteger>>(&mut self) -> Result<format::LenVec<I>> {
        self.length_encoded_vector(|input| input.unsigned_integer().map(I::from))
    }

    fn version_numbers(&mut self) -> Result<format::VersionNumbers> {
        self.length_encoded_vector(|input| input.unsigned_integer())
            .map(format::VersionNumbers)
    }

    fn module_identifier(&mut self) -> Result<format::ModuleIdentifier> {
        Ok(format::ModuleIdentifier {
            name: self.identifier()?,
            version: self.version_numbers()?,
        })
    }

    fn module_header(&mut self) -> Result<format::ModuleHeader> {
        let field_count = self.unsigned_integer()?;

        if field_count < format::MIN_HEADER_FIELD_COUNT
            || field_count > format::MAX_HEADER_FIELD_COUNT
        {
            return Err(Box::new(Error::InvalidHeaderFieldCount(field_count)));
        }

        let id = self.module_identifier()?;

        Ok(format::ModuleHeader { identifier: id })
    }

    fn type_tag(&mut self) -> Result<type_system::Tag> {
        Ok(type_system::Tag::try_from(self.byte()?).map_err(Error::InvalidTypeSignature)?)
    }

    fn fixed_integer_type(&mut self) -> Result<type_system::FixedInt> {
        Ok(type_system::FixedInt::try_from(
            type_system::Int::try_from(self.type_tag()?).map_err(Error::InvalidPrimitiveType)?,
        )
        .map_err(Error::InvalidFixedIntegerType)?)
    }

    fn type_signature(&mut self) -> Result<type_system::Any> {
        #[allow(deprecated)]
        match self.type_tag()? {
            type_system::Tag::Unit => unreachable!(),
            type_system::Tag::Struct => Ok(type_system::Any::Struct(self.unsigned_index()?)),
            type_system::Tag::NativePointer => Ok(type_system::Any::NativePointer(Box::new(
                self.type_signature()?,
            ))),
            type_system::Tag::FixedArray => {
                let length = self.unsigned_integer()?;
                Ok(type_system::Any::from(type_system::FixedArray::new(
                    self.type_signature()?,
                    length.0,
                )))
            }
            tag => Ok(type_system::Any::Primitive(
                type_system::Primitive::try_from(tag).map_err(Error::InvalidPrimitiveType)?,
            )),
        }
    }

    fn function_signature(&mut self) -> Result<format::FunctionSignature> {
        Ok(format::FunctionSignature {
            return_types: self.length_encoded_indices()?,
            parameter_types: self.length_encoded_indices()?,
        })
    }

    fn opcode(&mut self) -> Result<instruction_set::Opcode> {
        let mut opcode = 0u32;
        loop {
            let value = self.byte()?;
            opcode += u32::from(value);
            if value != instruction_set::Opcode::Continuation as u8 {
                return Ok(instruction_set::Opcode::try_from(opcode).map_err(Error::InvalidOpcode)?);
            }
        }
    }

    fn constant_integer(&mut self) -> Result<instruction_set::IntegerConstant> {
        use instruction_set::IntegerConstant;
        use type_system::FixedInt;

        Ok(match self.fixed_integer_type()? {
            FixedInt::U8 => IntegerConstant::U8(self.byte()?),
            FixedInt::S8 => IntegerConstant::S8(self.byte()? as i8),
            FixedInt::S16 => IntegerConstant::S16(i16::from_le_bytes(self.fixed_bytes()?)),
            FixedInt::U16 => IntegerConstant::U16(u16::from_le_bytes(self.fixed_bytes()?)),
            FixedInt::S32 => IntegerConstant::S32(i32::from_le_bytes(self.fixed_bytes()?)),
            FixedInt::U32 => IntegerConstant::U32(u32::from_le_bytes(self.fixed_bytes()?)),
            FixedInt::S64 => IntegerConstant::S64(i64::from_le_bytes(self.fixed_bytes()?)),
            FixedInt::U64 => IntegerConstant::U64(u64::from_le_bytes(self.fixed_bytes()?)),
        })
    }

    fn arithmetic_flags(&mut self) -> Result<instruction_set::ArithmeticFlags> {
        let bits = self.byte()?;
        let flags: instruction_set::ArithmeticFlags = unsafe { std::mem::transmute(bits) };
        if instruction_set::ArithmeticFlags::all().contains(flags) {
            Ok(flags)
        } else {
            Err(Box::new(Error::InvalidArithmeticFlags(bits)))
        }
    }

    fn basic_arithmetic_operation(&mut self) -> Result<instruction_set::BasicArithmeticOperation> {
        let flags = self.arithmetic_flags()?;
        Ok(instruction_set::BasicArithmeticOperation {
            overflow: instruction_set::OverflowBehavior::from(flags),
            x: self.unsigned_index()?,
            y: self.unsigned_index()?,
        })
    }

    fn instruction(&mut self) -> Result<instruction_set::Instruction> {
        use instruction_set::{Instruction, Opcode};

        #[allow(deprecated)]
        match self.opcode()? {
            Opcode::Nop => Ok(Instruction::Nop),
            Opcode::Ret => Ok(Instruction::Ret(self.length_encoded_indices()?)),
            Opcode::Phi => unreachable!(),
            Opcode::Select => {
                let condition = self.unsigned_index()?;
                let true_registers = self.length_encoded_indices()?;
                let mut false_registers = Vec::with_capacity(true_registers.len());

                for _ in 0..true_registers.len() {
                    false_registers.push(self.unsigned_index()?);
                }

                Ok(Instruction::Select {
                    condition,
                    values: instruction_set::SelectionValues {
                        true_registers,
                        false_registers,
                    },
                })
            }
            Opcode::Switch => {
                use instruction_set::IntegerConstant;
                use type_system::FixedInt;

                let comparison = self.unsigned_index()?;
                let comparison_type = self.fixed_integer_type()?;

                let default_target = self.unsigned_index()?;

                let mut target_lookup =
                    instruction_set::SwitchLookupTable::with_capacity(self.unsigned_length()?);

                let next_value: fn(&mut Self) -> Result<instruction_set::IntegerConstant> = {
                    match comparison_type {
                        FixedInt::S8 => |src| Ok(IntegerConstant::S8(src.byte()? as i8)),
                        FixedInt::U8 => |src| Ok(IntegerConstant::U8(src.byte()?)),
                        FixedInt::S16 => {
                            |src| Ok(IntegerConstant::S16(i16::from_le_bytes(src.fixed_bytes()?)))
                        }
                        FixedInt::U16 => {
                            |src| Ok(IntegerConstant::U16(u16::from_le_bytes(src.fixed_bytes()?)))
                        }
                        FixedInt::S32 => {
                            |src| Ok(IntegerConstant::S32(i32::from_le_bytes(src.fixed_bytes()?)))
                        }
                        FixedInt::U32 => {
                            |src| Ok(IntegerConstant::U32(u32::from_le_bytes(src.fixed_bytes()?)))
                        }
                        FixedInt::S64 => {
                            |src| Ok(IntegerConstant::S64(i64::from_le_bytes(src.fixed_bytes()?)))
                        }
                        FixedInt::U64 => {
                            |src| Ok(IntegerConstant::U64(u64::from_le_bytes(src.fixed_bytes()?)))
                        }
                    }
                };

                for _ in 0..target_lookup.len() {
                    let value = next_value(self)?;
                    let target = self.unsigned_index()?;
                    if !target_lookup.insert(value, target) {
                        return Err(Box::new(Error::DuplicateSwitchBranch(value)));
                    }
                }

                Ok(Instruction::Switch {
                    comparison,
                    comparison_type,
                    default_target,
                    target_lookup,
                })
            }
            Opcode::Br => Ok(Instruction::Br {
                target: self.unsigned_index()?,
                input_registers: self.length_encoded_indices()?,
            }),
            Opcode::BrIf => Ok(Instruction::BrIf {
                condition: self.unsigned_index()?,
                true_branch: self.unsigned_index()?,
                true_inputs: self.length_encoded_indices()?,
                false_branch: self.unsigned_index()?,
                false_inputs: self.length_encoded_indices()?,
            }),
            Opcode::Call => Ok(Instruction::Call(
                format::instruction_set::CallInstruction {
                    function: self.unsigned_index()?,
                    arguments: self.length_encoded_indices()?,
                },
            )),
            Opcode::Add => Ok(Instruction::Add(self.basic_arithmetic_operation()?)),
            Opcode::Sub => Ok(Instruction::Sub(self.basic_arithmetic_operation()?)),
            Opcode::Mul => Ok(Instruction::Mul(self.basic_arithmetic_operation()?)),
            // Opcode::Div => Ok(Instruction::Div(division_operation(src, size)?)),
            Opcode::And => Ok(Instruction::And {
                x: self.unsigned_index()?,
                y: self.unsigned_index()?,
            }),
            Opcode::Or => Ok(Instruction::And {
                x: self.unsigned_index()?,
                y: self.unsigned_index()?,
            }),
            Opcode::Not => Ok(Instruction::Not(self.unsigned_index()?)),
            Opcode::Xor => Ok(Instruction::And {
                x: self.unsigned_index()?,
                y: self.unsigned_index()?,
            }),
            // Opcode::ShL => Ok(Instruction::ShL(bitwise_shift_operation(src, size)?)),
            // Opcode::ShR => Ok(Instruction::ShR(bitwise_shift_operation(src, size)?)),
            // Opcode::RotL => Ok(Instruction::RotL(bitwise_shift_operation(src, size)?)),
            // Opcode::RotR => Ok(Instruction::RotR(bitwise_shift_operation(src, size)?)),
            Opcode::ConstI => Ok(Instruction::ConstI(self.constant_integer()?)),
            Opcode::ConvI => Ok(Instruction::ConvI {
                target_type: type_system::Int::try_from(self.type_tag()?)
                    .map_err(Error::InvalidPrimitiveType)?,
                overflow: instruction_set::OverflowBehavior::try_from(self.byte()?)
                    .map_err(Error::InvalidOverflowBehavior)?,
                operand: self.unsigned_index()?,
            }),
            Opcode::Rotate => Ok(Instruction::Rotate {
                direction: instruction_set::RotationDirection::try_from(self.byte()?)
                    .map_err(Error::InvalidRotationDirection)?,
                value: self.unsigned_index()?,
                amount: self.unsigned_index()?,
            }),
            Opcode::Cmp => Ok(Instruction::Cmp {
                x: self.unsigned_index()?,
                kind: instruction_set::ComparisonKind::try_from(self.byte()?)
                    .map_err(Error::InvalidComparisonKind)?,
                y: self.unsigned_index()?,
            }),
            Opcode::Field => Ok(Instruction::Field {
                field: self.unsigned_index()?,
                object: self.unsigned_index()?,
            }),
            Opcode::MemInit => {
                let destination = self.unsigned_index()?;
                let flags = instruction_set::MemoryInitializationFlags::try_from(self.byte()?)
                    .map_err(Error::InvalidMemoryInitializationFlags)?;

                Ok(Instruction::MemInit {
                    destination,
                    source: match flags {
                        instruction_set::MemoryInitializationFlags::FromData => {
                            instruction_set::MemoryInitializationSource::FromData(
                                self.unsigned_index()?,
                            )
                        }
                    },
                })
            }
            Opcode::Alloca => Ok(Instruction::Alloca {
                amount: self.unsigned_index()?,
                element_type: self.unsigned_index()?,
            }),
            Opcode::Break => Ok(Instruction::Break),
            Opcode::Continuation => unreachable!(),
            bad => todo!(
                "TODO: Add support for parsing of more instructions such as {:?}",
                bad
            ),
        }
    }

    fn byte_flags<B, C: FnOnce(u8) -> Option<B>, E: FnOnce(u8) -> Error>(
        &mut self,
        converter: C,
        error: E,
    ) -> Result<B> {
        let bits = self.byte()?;
        converter(bits).ok_or_else(|| Box::new(error(bits)))
    }

    fn code_block(&mut self) -> Result<format::CodeBlock> {
        let flags = self.byte_flags(flags::CodeBlock::from_bits, Error::InvalidCodeBlockFlags)?;

        Ok(format::CodeBlock {
            input_registers: self.length_encoded_indices()?,
            temporary_registers: self.length_encoded_indices()?,
            exception_handler: if flags.is_empty() { None } else { todo!() },
            instructions: {
                let length = self.unsigned_length()?;
                let buffer = self.many_bytes(length)?;
                format::LenBytes(
                    self.change_input(buffer.as_slice())
                        .length_encoded_vector(|src| src.instruction())?,
                )
            },
        })
    }

    fn function_body(&mut self) -> Result<format::Code> {
        Ok(format::Code {
            entry_block: self.code_block()?,
            blocks: self.length_encoded_vector(|src| src.code_block())?,
        })
    }

    fn data_array(&mut self) -> Result<format::DataArray> {
        let length = self.unsigned_length()?;
        self.many_bytes(length)
            .map(|data| format::DataArray(format::LenVec(data)))
    }

    fn namespace_definition(&mut self) -> Result<format::Namespace> {
        let name = self.unsigned_index()?;
        let flags = self.byte_flags(flags::Namespace::from_bits, Error::InvalidNamespaceFlags)?;

        Ok(format::Namespace {
            name,
            parent: if flags.contains(flags::Namespace::HAS_PARENT) {
                Some(self.unsigned_index()?)
            } else {
                None
            },
            structs: self.length_encoded_indices()?,
            globals: self.length_encoded_indices()?,
            functions: self.length_encoded_indices()?,
        })
    }

    fn module_import(&mut self) -> Result<format::ModuleImport> {
        Ok(format::ModuleImport {
            identifier: self.module_identifier()?,
            hash: {
                match self.unsigned_length()? {
                    0usize => None,
                    256usize => Some(Box::new(self.fixed_bytes::<256>()?)),
                    length => {
                        return Err(Box::new(Error::InvalidByteLength {
                            expected: 256,
                            actual: length,
                        }))
                    }
                }
            },
        })
    }

    fn struct_import(&mut self) -> Result<format::StructImport> {
        Ok(format::StructImport {
            module: self.unsigned_index()?,
            symbol: self.unsigned_index()?,
        })
    }

    //global_import

    fn field_import(&mut self) -> Result<format::FieldImport> {
        Ok(format::FieldImport {
            owner: self.unsigned_index()?,
            symbol: self.unsigned_index()?,
            signature: self.unsigned_index()?,
        })
    }

    fn function_import(&mut self) -> Result<format::FunctionImport> {
        Ok(format::FunctionImport {
            module: self.unsigned_index()?,
            symbol: self.unsigned_index()?,
            signature: self.unsigned_index()?,
        })
    }

    fn struct_definition(&mut self) -> Result<format::Struct> {
        let name = self.unsigned_index()?;
        let flags = self.byte_flags(flags::Struct::from_bits, Error::InvalidStructFlags)?;

        Ok(format::Struct {
            name,
            is_export: flags.is_export(),
            symbol: self.unsigned_index()?,
            layout: self.unsigned_index()?,
            fields: self.length_encoded_indices()?,
        })
    }

    //global_definition

    fn field_definition(&mut self) -> Result<format::Field> {
        let owner = self.unsigned_index()?;
        let name = self.unsigned_index()?;
        let flags = self.byte_flags(flags::Field::from_bits, Error::InvalidFieldFlags)?;

        Ok(format::Field {
            owner,
            name,
            is_export: flags.is_export(),
            symbol: self.unsigned_index()?,
            signature: self.unsigned_index()?,
        })
    }

    fn function_definition(&mut self) -> Result<format::Function> {
        let name = self.unsigned_index()?;
        let signature = self.unsigned_index()?;
        let flags = self.byte_flags(flags::Function::from_bits, Error::InvalidFunctionFlags)?;

        Ok(format::Function {
            name,
            signature,
            is_export: flags.is_export(),
            symbol: self.unsigned_index()?,
            body: if flags.contains(flags::Function::IS_EXTERNAL) {
                format::FunctionBody::External {
                    library: self.unsigned_index()?,
                    entry_point_name: self.unsigned_index()?,
                }
            } else {
                format::FunctionBody::Defined(self.unsigned_index()?)
            },
        })
    }

    fn struct_layout(&mut self) -> Result<format::StructLayout> {
        Ok(
            match self.byte_flags(
                |bits| flags::StructLayout::try_from(bits).ok(),
                Error::InvalidStructLayoutFlags,
            )? {
                flags::StructLayout::Unspecified => format::StructLayout::Unspecified,
                _ => todo!("Parsing of specific struct layouts is not yet supported"),
            },
        )
    }

    fn magic_bytes<E: FnOnce() -> Error>(&mut self, magic: &[u8], error: E) -> Result<()> {
        let actual = self.many_bytes(magic.len())?;
        if actual == magic {
            Ok(())
        } else {
            Err(Box::new(error()))
        }
    }

    fn integer_size(&mut self) -> Result<numeric::IntegerSize> {
        self.byte().and_then(|value| match value {
            0 => Ok(numeric::IntegerSize::I1),
            1 => Ok(numeric::IntegerSize::I2),
            2 => Ok(numeric::IntegerSize::I4),
            _ => Err(Box::new(Error::InvalidIntegerSize(value))),
        })
    }

    fn double_length_encoded<T, F: FnMut(&mut Input<'_, &[u8]>) -> Result<T>>(
        &mut self,
        parser: F,
    ) -> Result<format::LenVecBytes<T>> {
        let length = self.unsigned_length()?;
        Ok(format::LenBytes(if length > 0usize {
            let mut buffer = self.buffer_pool.rent_with_length(length);
            self.fill_buffer(&mut buffer)?;
            let buffer_slice: &[u8] = &mut buffer;
            self.change_input(buffer_slice)
                .length_encoded_vector(parser)?
        } else {
            format::LenVec(Vec::new())
        }))
    }

    fn module_data<T, D: FnOnce() -> T, F: FnOnce(&mut Input<'_, &[u8]>) -> Result<T>>(
        &mut self,
        data_vectors: &[Vec<u8>],
        index: usize,
        default: D,
        parser: F,
    ) -> Result<format::LenBytes<T>> {
        data_vectors
            .get(index)
            .and_then(|data| {
                if data.is_empty() {
                    None
                } else {
                    let mut data_input = self.change_input(data.as_slice());
                    Some(parser(&mut data_input))
                }
            })
            .unwrap_or_else(|| Ok(default()))
            .map(format::LenBytes)
    }

    fn module_data_or_default<T: Default, F: FnOnce(&mut Input<'_, &[u8]>) -> Result<T>>(
        &mut self,
        data_vectors: &[Vec<u8>],
        index: usize,
        parser: F,
    ) -> Result<format::LenBytes<T>> {
        self.module_data(data_vectors, index, T::default, parser)
    }
}

/// Parses a binary module.
pub fn parse_module<R: Read>(input: &mut R) -> Result<(format::Module, format::ModuleHash)> {
    let buffers = buffers::BufferPool::new();
    let mut hasher = sha2::Sha256::new();

    let mut source = Input {
        source: input,
        // Integer size is updated once parsed.
        integer_size: numeric::IntegerSize::default(),
        buffer_pool: &buffers,
        hasher: &mut hasher,
    };

    source.magic_bytes(format::MAGIC, || Error::InvalidModuleMagic)?;
    source.integer_size = source.integer_size()?;
    let format_version = format::FormatVersion {
        major: source.unsigned_integer()?,
        minor: source.unsigned_integer()?,
    };

    if !format_version.is_supported() {
        return Err(Box::new(Error::UnsupportedFormatVersion(format_version)));
    }

    let data_count = source.unsigned_integer()?;

    if data_count < format::MIN_MODULE_DATA_COUNT || data_count > format::MAX_MODULE_DATA_COUNT {
        return Err(Box::new(Error::InvalidDataVectorCount(data_count)));
    }

    let mut data_vectors: Vec<Vec<u8>> = Vec::with_capacity(data_count.try_into().unwrap());

    for _ in 0u32..(data_count.0) {
        let length = source.unsigned_length()?;
        data_vectors.push(source.many_bytes(length)?);
    }

    let buffers = buffers::BufferPool::new();

    let module = format::Module {
        integer_size: source.integer_size,
        format_version,
        // Header is always present.
        header: format::LenBytes(
            source
                .change_input(data_vectors[0].as_slice())
                .module_header()?,
        ),
        identifiers: source.module_data(
            &data_vectors,
            1,
            || format::LenVec(Vec::new()),
            |data| data.length_encoded_vector(|src| src.identifier()),
        )?,
        namespaces: source.module_data(
            &data_vectors,
            2,
            || format::LenVec(Vec::new()),
            |data| data.length_encoded_vector(|src| src.namespace_definition()),
        )?,
        type_signatures: source.module_data(
            &data_vectors,
            3,
            || format::LenVec(Vec::new()),
            |data| data.length_encoded_vector(|src| src.type_signature()),
        )?,
        function_signatures: source.module_data_or_default(&data_vectors, 4, |data| {
            data.length_encoded_vector(|src| src.function_signature())
        })?,
        function_bodies: source.module_data(
            &data_vectors,
            5,
            || format::LenVec(Vec::new()),
            |data| data.length_encoded_vector(|src| src.function_body()),
        )?,
        data: source.module_data_or_default(&data_vectors, 6, |data| {
            data.length_encoded_vector(|src| src.data_array())
        })?,
        imports: source.module_data(
            &data_vectors,
            7,
            || format::ModuleImports {
                imported_modules: format::LenBytes(format::LenVec(Vec::new())),
                imported_structs: format::LenBytes(format::LenVec(Vec::new())),
                imported_globals: format::LenBytes(format::LenVec(Vec::new())),
                imported_fields: format::LenBytes(format::LenVec(Vec::new())),
                imported_functions: format::LenBytes(format::LenVec(Vec::new())),
            },
            |data| {
                Ok(format::ModuleImports {
                    imported_modules: data.double_length_encoded(|src| src.module_import())?,
                    imported_structs: data.double_length_encoded(|src| src.struct_import())?,
                    imported_globals: data.double_length_encoded(|src| todo!())?,
                    imported_fields: data.double_length_encoded(|src| src.field_import())?,
                    imported_functions: data.double_length_encoded(|src| src.function_import())?,
                })
            },
        )?,
        definitions: source.module_data(
            &data_vectors,
            8,
            || format::ModuleDefinitions {
                defined_structs: format::LenBytes(format::LenVec(Vec::new())),
                defined_globals: format::LenBytes(format::LenVec(Vec::new())),
                defined_fields: format::LenBytes(format::LenVec(Vec::new())),
                defined_functions: format::LenBytes(format::LenVec(Vec::new())),
            },
            |data| {
                Ok(format::ModuleDefinitions {
                    defined_structs: data.double_length_encoded(|src| src.struct_definition())?,
                    defined_globals: data.double_length_encoded(|src| todo!())?,
                    defined_fields: data.double_length_encoded(|src| src.field_definition())?,
                    defined_functions: data
                        .double_length_encoded(|src| src.function_definition())?,
                })
            },
        )?,
        struct_layouts: source.module_data(
            &data_vectors,
            9,
            || format::LenVec(Vec::new()),
            |data| data.length_encoded_vector(|src| src.struct_layout()),
        )?,
        entry_point: source.module_data(
            &data_vectors,
            10,
            || None,
            |data| data.unsigned_index().map(Some),
        )?,
    };

    Ok((
        module,
        Box::new(
            hasher
                .finalize()
                .as_slice()
                .try_into()
                .expect("hash length should be 256 bytes"),
        ),
    ))
}
