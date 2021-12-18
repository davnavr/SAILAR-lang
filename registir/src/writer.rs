use crate::format::instruction_set::{Instruction, Opcode};
use crate::{format, format::instruction_set};

#[derive(Debug)]
#[non_exhaustive]
pub enum WriteError {
    VectorTooLarge(usize),
    IoError(std::io::Error),
}

impl std::fmt::Display for WriteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VectorTooLarge(size) => write!(f, "{} is not a valid size for a vector", size),
            Self::IoError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for WriteError {}

pub type WriteResult = Result<(), WriteError>;

trait BinWrite {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult;
}

impl<T: BinWrite + Copy> BinWrite for &T {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        (*self).write(out)
    }
}

impl BinWrite for &[u8] {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        match out.write_all(self) {
            Ok(()) => Ok(()),
            Err(err) => Err(WriteError::IoError(err)),
        }
    }
}

impl BinWrite for u8 {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        (&[self]).write(out)
    }
}

impl BinWrite for &Vec<u8> {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let bytes: &[u8] = self;
        bytes.write(out)
    }
}

impl BinWrite for format::uvarint {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::uvarint(value) = self;
        let bytes = value.to_le_bytes();
        let sliced = if value < 0x80u64 {
            &bytes[0..0]
        } else {
            panic!("Writing of unsigned integer {} is not supported", value)
        };

        sliced.write(out)
    }
}

macro_rules! index_writer {
    ($name: ty) => {
        impl BinWrite for $name {
            fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
                self.index().write(out)
            }
        }
    };
}

index_writer!(format::IdentifierIndex);
index_writer!(format::NamespaceIndex);
index_writer!(format::TypeSignatureIndex);
index_writer!(format::MethodSignatureIndex);
index_writer!(format::CodeIndex);
index_writer!(format::DataIndex);
index_writer!(format::ModuleIndex);
index_writer!(format::TypeDefinitionIndex);
index_writer!(format::FieldIndex);
index_writer!(format::MethodIndex);
index_writer!(format::TypeLayoutIndex);
index_writer!(format::CodeBlockIndex);
index_writer!(format::InputRegisterIndex);
index_writer!(format::TemporaryRegisterIndex);

impl BinWrite for format::varint {
    // TODO: Check that this actually works as intended when writing signed integers.
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::varint(value) = self;
        let converted = if value >= 0xC0 && value <= 0x3F {
            (value as u8 & 0b0111_1111u8) as u64
        } else {
            panic!("Writing of signed integer {} is not supported", value)
        };

        format::uvarint(converted).write(out)
    }
}

impl BinWrite for instruction_set::BlockOffset {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let instruction_set::BlockOffset(offset) = self;
        offset.write(out)
    }
}

impl BinWrite for usize {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        match u64::try_from(self) {
            Ok(length) => format::uvarint(length).write(out),
            Err(_) => Err(WriteError::VectorTooLarge(self)),
        }
    }
}

impl BinWrite for &format::Identifier {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let bytes = self.bytes();
        bytes.len().write(out)?;
        bytes.write(out)
    }
}

impl BinWrite for &format::FormatVersion {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.major.write(out)?;
        self.minor.write(out)
    }
}

impl<'a, D> BinWrite for &'a format::ByteLengthEncoded<D>
where
    &'a D: BinWrite,
{
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let mut buffer: Vec<u8> = Vec::new();
        let format::ByteLengthEncoded(ref data) = self;
        data.write(&mut buffer)?;
        buffer.len().write(out)?;
        buffer.as_slice().write(out)
    }
}

impl<'a, D: BinWrite> BinWrite for &'a format::ByteLengthEncoded<Option<D>>
where
    &'a D: BinWrite,
{
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::ByteLengthEncoded(ref wrapped) = self;
        match wrapped {
            Some(ref data) => format::ByteLengthEncoded(data).write(out),
            None => format::uvarint::default().write(out),
        }
    }
}

impl<'a, T> BinWrite for &'a format::LengthEncodedVector<T>
where
    &'a T: BinWrite,
{
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::LengthEncodedVector(items) = self;
        items.len().write(out)?;
        for e in items {
            e.write(out)?;
        }
        Ok(())
    }
}

impl BinWrite for &format::VersionNumbers {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::VersionNumbers(numbers) = self;
        numbers.write(out)
    }
}

impl BinWrite for &format::ModuleIdentifier {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.name.write(out)?;
        self.version.write(out)
    }
}

impl BinWrite for &format::ModuleHeader {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.field_count().write(out)?;
        self.identifier.write(out)
    }
}

impl BinWrite for format::TypeTag {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        (self as u8).write(out)
    }
}

impl BinWrite for &format::SimpleType {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        format::TypeTagged::tag(self).write(out)?;
        match self {
            format::SimpleType::Primitive(_) => Ok(()),
            format::SimpleType::Defined(index) => (*index).write(out),
            format::SimpleType::NativePointer(pointee) => pointee.write(out),
        }
    }
}

impl BinWrite for &format::HeapType {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        format::TypeTagged::tag(self).write(out)?;
        match self {
            format::HeapType::ObjRef(format::SimpleType::Defined(index)) => index.write(out),
            format::HeapType::ArrayRef(element_type)
            | format::HeapType::HeapPointer(element_type) => element_type.write(out),
            format::HeapType::ObjRef(simple_type) => simple_type.write(out),
            format::HeapType::Val(_) | format::HeapType::AnyRef => Ok(()),
        }
    }
}

impl BinWrite for &format::AnyType {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        match self {
            format::AnyType::Val(simple_type) => simple_type.write(out),
            format::AnyType::Heap(heap_type) => heap_type.write(out),
            format::AnyType::GargbageCollectedPointer(element_type) => {
                format::TypeTag::GarbageCollectedPointer.write(out)?;
                element_type.write(out)
            }
        }
    }
}

impl BinWrite for &format::MethodSignature {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.return_types.write(out)?;
        self.parameter_types.write(out)
    }
}

impl BinWrite for Opcode {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let mut value = self as usize;

        (value as u8).write(out)?;
        value -= Opcode::Continuation as usize;

        while value > 0 {
            (value as u8).write(out)?;
            value -= Opcode::Continuation as usize;
        }

        Ok(())
    }
}

struct RegisterIndexVectorWrite<'t> {
    input_register_count: format::uvarint,
    indices: &'t format::LengthEncodedVector<instruction_set::RegisterIndex>,
}

impl<'t> BinWrite for &RegisterIndexVectorWrite<'t> {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::LengthEncodedVector(indices) = self.indices;
        indices.len().write(out)?;
        for index in indices {
            index.index(self.input_register_count).write(out)?;
        }
        Ok(())
    }
}

impl BinWrite for &format::CodeBlock {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.flags().bits().write(out)?;
        self.input_register_count.write(out)?;

        // Flags already indicate if exception handler is present or if exception register is used.
        if let Some(exception_handler) = &self.exception_handler {
            exception_handler.catch_block.write(out)?;
            if let Some(exception_register) = exception_handler.exception_register {
                exception_register.write(out)?;
            }
        }

        let format::ByteLengthEncoded(format::LengthEncodedVector(code)) = &self.instructions;
        for instruction in code {
            instruction.opcode().write(out)?;
            match instruction {
                Instruction::Nop => (),
                Instruction::Ret(registers) => {
                    RegisterIndexVectorWrite {
                        input_register_count: self.input_register_count,
                        indices: registers,
                    }
                    .write(out)?;
                }
            };
        }

        Ok(())
    }
}

impl BinWrite for &format::Code {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.entry_block.write(out)?;
        self.blocks.write(out)
    }
}

impl BinWrite for &format::DataArray {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        let format::DataArray(bytes) = self;
        bytes.len().write(out)?;
        bytes.write(out)
    }
}

impl BinWrite for &format::TypeDefinitionImport {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.module.write(out)?;
        self.name.write(out)?;
        self.namespace.write(out)
    }
}

impl BinWrite for &format::FieldImport {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.owner.write(out)?;
        self.name.write(out)?;
        self.signature.write(out)
    }
}

impl BinWrite for &format::MethodImport {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.owner.write(out)?;
        self.name.write(out)?;
        self.signature.write(out)
    }
}

impl BinWrite for &format::ModuleImports {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.imported_modules.write(out)?;
        self.imported_types.write(out)?;
        self.imported_fields.write(out)?;
        self.imported_methods.write(out)
    }
}

impl BinWrite for &format::MethodOverride {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.declaration.write(out)?;
        self.implementation.write(out)
    }
}

impl BinWrite for &format::TypeDefinition {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.name.write(out)?;
        self.namespace.write(out)?;
        (self.visibility as u8).write(out)?;
        self.flags.bits().write(out)?;
        self.layout.write(out)?;
        self.inherited_types.write(out)?;
        self.fields.write(out)?;
        self.methods.write(out)?;
        self.vtable.write(out)
    }
}

impl BinWrite for &format::Field {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.owner.write(out)?;
        self.name.write(out)?;
        (self.visibility as u8).write(out)?;
        self.flags.bits().write(out)?;
        self.signature.write(out)
    }
}

impl BinWrite for &format::Method {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.owner.write(out)?;
        self.name.write(out)?;
        (self.visibility as u8).write(out)?;
        self.flags.bits().write(out)?;
        self.implementation_flags().bits().write(out)?;
        self.signature.write(out)?;

        match self.body {
            format::MethodBody::Defined(_) | format::MethodBody::Abstract => Ok(()),
            format::MethodBody::External {
                library,
                entry_point_name,
            } => {
                library.write(out)?;
                entry_point_name.write(out)
            }
        }
    }
}

impl BinWrite for &format::ModuleDefinitions {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.defined_types.write(out)?;
        self.defined_fields.write(out)?;
        self.defined_methods.write(out)
    }
}

impl BinWrite for &format::FieldOffset {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        self.field.write(out)?;
        self.offset.write(out)
    }
}

impl BinWrite for &format::TypeDefinitionLayout {
    fn write<W: std::io::Write>(self, out: &mut W) -> WriteResult {
        (self.flags() as u8).write(out)?;

        match self {
            format::TypeDefinitionLayout::Unspecified
            | format::TypeDefinitionLayout::Sequential(None) => Ok(()),
            format::TypeDefinitionLayout::Sequential(Some(size)) => size.write(out),
            format::TypeDefinitionLayout::Explicit {
                size,
                field_offsets,
            } => {
                size.write(out)?;
                for offset in field_offsets {
                    offset.write(out)?;
                }
                Ok(())
            }
        }
    }
}

/// Writes a binary module.
pub fn write<W: std::io::Write>(module: &format::Module, out: &mut W) -> WriteResult {
    format::MAGIC.write(out)?;
    // NOTE: For all top level byte encoding things, could share Vec<u8> to reduce allocations.
    module.format_version.write(out)?;
    module.data_count().write(out)?;
    module.header.write(out)?;
    module.identifiers.write(out)?;
    module.namespaces.write(out)?;
    module.type_signatures.write(out)?;
    module.method_signatures.write(out)?;
    module.method_bodies.write(out)?;
    module.data_arrays.write(out)?;
    module.imports.write(out)?;
    module.definitions.write(out)?;
    module.entry_point.write(out)?;
    module.type_layouts.write(out)
}
