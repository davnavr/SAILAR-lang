use crate::{format, format::instruction_set};
use crate::format::instruction_set::{Opcode, Instruction};

#[non_exhaustive]
pub enum WriteError {
    VectorTooLarge(usize),
    IoError(std::io::Error),
}

pub type WriteResult = Result<(), WriteError>;

trait BinWrite {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult;
}

impl BinWrite for &[u8] {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        match destination.write_all(self) {
            Ok(()) => Ok(()),
            Err(err) => Err(WriteError::IoError(err)),
        }
    }
}

impl BinWrite for u8 {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let buffer: &[u8] = &[ *self ];
        buffer.write(destination)
    }
}

impl BinWrite for Vec<u8> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let bytes: &[u8] = self;
        bytes.write(destination)
    }
}

impl BinWrite for format::uvarint {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::uvarint(value) = *self;
        let bytes = value.to_le_bytes();
        let sliced =
            if value < 0x80u64 {
                &bytes[0 .. 0]
            }
            else {
                panic!("Writing of unsigned integer {} is not supported", value)
            };

        sliced.write(destination)
    }
}

impl<T: format::Index> BinWrite for T {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.index().write(destination)
    }
}

impl BinWrite for format::varint {
    // TODO: Check that this actually works as intended when writing signed integers.
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::varint(value) = *self;
        let converted =
            if value >= 0xC0 && value <= 0x3F {
                (value as u8 & 0b0111_1111u8) as u64
            }
            else {
                panic!("Writing of signed integer {} is not supported", value)
            };

        format::uvarint(converted).write(destination)
    }
}

impl BinWrite for instruction_set::BlockOffset {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let instruction_set::BlockOffset(offset) = self;
        offset.write(destination)
    }
}

impl BinWrite for usize {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        match u64::try_from(*self) {
            Ok(length) => format::uvarint(length).write(destination),
            Err(_) => Err(WriteError::VectorTooLarge(*self))
        }
    }
}

impl BinWrite for format::Identifier {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let bytes = self.bytes();
        bytes.len().write(destination)?;
        bytes.write(destination)
    }
}

impl BinWrite for format::FormatVersion {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.major.write(destination)?;
        self.minor.write(destination)
    }
}

impl<D: BinWrite> BinWrite for format::ByteLengthEncoded<D> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let mut buffer: Vec<u8> = Vec::new();
        let format::ByteLengthEncoded(data) = self;
        data.write(&mut buffer)?;
        buffer.len().write(destination)?;
        buffer.as_slice().write(destination)
    }
}

impl<T: BinWrite> BinWrite for format::LengthEncodedVector<T> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::LengthEncodedVector(items) = self;
        items.len().write(destination)?;
        for e in items {
            e.write(destination)?;
        }
        Ok(())
    }
}

impl BinWrite for format::VersionNumbers {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::VersionNumbers(numbers) = self;
        numbers.write(destination)
    }
}

impl BinWrite for format::ModuleIdentifier {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.name.write(destination)?;
        self.version.write(destination)
    }
}

impl BinWrite for format::ModuleHeader {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.field_count().write(destination)?;
        self.identifier.write(destination)
    }
}

impl BinWrite for format::TypeTag {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        (*self as u8).write(destination)
    }
}

impl BinWrite for format::SimpleType {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        format::TypeTagged::tag(self).write(destination)?;
        match self {
            format::SimpleType::Primitive(_) => Ok(()),
            format::SimpleType::Defined(index) => index.write(destination),
            format::SimpleType::NativePointer(pointee) => pointee.write(destination)
        }
    }
}

impl BinWrite for format::HeapType {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        format::TypeTagged::tag(self).write(destination)?;
        match self {
            format::HeapType::ObjRef(format::SimpleType::Defined(index)) => index.write(destination),
            format::HeapType::ArrayRef(element_type) |
            format::HeapType::HeapPointer(element_type) => element_type.write(destination),
            format::HeapType::ObjRef(simple_type) => simple_type.write(destination),
            format::HeapType::Val(_) |
            format::HeapType::AnyRef => Ok(()),
        }
    }
}

impl BinWrite for format::AnyType {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        match self {
            format::AnyType::Val(simple_type) => simple_type.write(destination),
            format::AnyType::Heap(heap_type) => heap_type.write(destination),
            format::AnyType::GargbageCollectedPointer(element_type) => {
                format::TypeTag::GarbageCollectedPointer.write(destination)?;
                element_type.write(destination)
            },
        }
    }
}

impl BinWrite for format::MethodSignature {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.return_types.write(destination)?;
        self.parameter_types.write(destination)
    }
}

impl BinWrite for Opcode {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let mut value = *self as usize;

        (value as u8).write(destination)?;
        value = value - Opcode::Continuation as usize;

        while value > 0 {
            (value as u8).write(destination)?;
            value = value - Opcode::Continuation as usize;
        }

        Ok(())
    }
}

struct RegisterIndexVectorWrite<'t> {
    input_register_count: format::uvarint,
    indices: &'t format::LengthEncodedVector<instruction_set::RegisterIndex>,
}

impl<'t> BinWrite for RegisterIndexVectorWrite<'t> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::LengthEncodedVector(indices) = self.indices;
        indices.len().write(destination)?;
        for index in indices {
            index.index(self.input_register_count).write(destination)?;
        };
        Ok(())
    }
}

impl BinWrite for format::CodeBlock {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        (self.flags() as u8).write(destination)?;
        self.input_register_count.write(destination)?;

        // Flags already indicate if exception handler is present or if exception register is used.
        if let Some(exception_handler) = &self.exception_handler {
            exception_handler.catch_block.write(destination)?;
            if let Some(exception_register) = exception_handler.exception_register {
                exception_register.write(destination)?;
            }
        }

        let format::ByteLengthEncoded(format::LengthEncodedVector(code)) = &self.instructions;
        for instruction in code {
            instruction.opcode().write(destination)?;
            match instruction {
                Instruction::Nop => (),
                Instruction::Ret(registers) => {
                    RegisterIndexVectorWrite {
                        input_register_count: self.input_register_count, 
                        indices: registers
                    }
                    .write(destination)?;
                },
            };
        };

        Ok(())
    }
}

impl BinWrite for format::Code {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.entry_block.write(destination)?;
        self.blocks.write(destination)
    }
}

impl BinWrite for format::DataArray {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::DataArray(bytes) = self;
        bytes.len().write(destination)?;
        bytes.write(destination)
    }
}

/// Writes a binary module to the specified [`destination`].
pub fn write<Destination: std::io::Write>(module: &format::Module, destination: &mut Destination) -> WriteResult {
    format::MAGIC.write(destination)?;
    module.format_version.write(destination)?;
    module.data_count().write(destination)?;
    module.header.write(destination)?;
    module.identifiers.write(destination)?;
    module.namespaces.write(destination)?;
    module.type_signatures.write(destination)?;
    module.method_signatures.write(destination)?;
    module.method_bodies.write(destination)?;
    module.data_arrays.write(destination)?;
    unimplemented!()
}
