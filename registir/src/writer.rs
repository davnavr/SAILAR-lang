use crate::{
    buffers, format,
    format::{instruction_set, numeric, structures, type_system},
};

#[derive(Debug)]
#[non_exhaustive]
pub enum WriteError {
    VectorTooLarge(usize),
    InputOutputError(std::io::Error),
}

impl std::fmt::Display for WriteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VectorTooLarge(size) => write!(f, "{} is not a valid size for a vector", size),
            Self::InputOutputError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for WriteError {}

pub type WriteResult = Result<(), WriteError>;

fn write_bytes<W: std::io::Write>(out: &mut W, bytes: &[u8]) -> WriteResult {
    match out.write_all(bytes) {
        Ok(()) => Ok(()),
        Err(err) => Err(WriteError::InputOutputError(err)),
    }
}

fn write<W: std::io::Write>(out: &mut W, value: u8) -> WriteResult {
    write_bytes(out, &[value])
}

fn unsigned_integer<W: std::io::Write>(
    out: &mut W,
    numeric::UInteger(value): numeric::UInteger,
    size: numeric::IntegerSize,
) -> WriteResult {
    write_bytes(out, &u32::to_le_bytes(value)[..(size.size() as usize)])
}

fn unsigned_index<W: std::io::Write, I: Into<numeric::UInteger>>(
    out: &mut W,
    index: I,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_integer(out, index.into(), size)
}

fn signed_integer<W: std::io::Write>(
    out: &mut W,
    numeric::SInteger(value): numeric::SInteger,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_integer(out, numeric::UInteger(value as u32), size)
}

fn block_offset<W: std::io::Write>(
    out: &mut W,
    instruction_set::BlockOffset(offset): instruction_set::BlockOffset,
    size: numeric::IntegerSize,
) -> WriteResult {
    signed_integer(out, offset, size)
}

fn unsigned_length<W: std::io::Write>(
    out: &mut W,
    length: usize,
    size: numeric::IntegerSize,
) -> WriteResult {
    match u32::try_from(length) {
        Ok(value) => unsigned_integer(out, numeric::UInteger(value), size),
        Err(_) => Err(WriteError::VectorTooLarge(length)),
    }
}

fn identifier<W: std::io::Write>(
    out: &mut W,
    id: &format::Identifier,
    size: numeric::IntegerSize,
) -> WriteResult {
    let bytes = id.as_bytes();
    debug_assert!(!bytes.is_empty());
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn format_version<W: std::io::Write>(
    out: &mut W,
    version: &format::FormatVersion,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_integer(out, version.major, size)?;
    unsigned_integer(out, version.minor, size)
}

fn byte_length_encoded<
    D,
    R: FnOnce(&mut Vec<u8>, &D, &buffers::BufferPool) -> WriteResult,
    W: std::io::Write,
>(
    out: &mut W,
    structures::ByteLengthEncoded(data): structures::ByteLengthEncoded<&D>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> WriteResult {
    let mut buffer = buffer_pool.rent();
    let bytes: &mut Vec<u8> = &mut buffer;
    writer(bytes, data, buffer_pool)?;
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn byte_length_optional<
    D,
    R: FnOnce(&mut Vec<u8>, &D, &buffers::BufferPool) -> WriteResult,
    W: std::io::Write,
>(
    out: &mut W,
    structures::ByteLengthEncoded(ref wrapped): structures::ByteLengthEncoded<&Option<D>>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> WriteResult {
    match wrapped {
        Some(data) => byte_length_encoded(
            out,
            structures::ByteLengthEncoded(data),
            size,
            buffer_pool,
            writer,
        ),
        None => unsigned_integer(out, numeric::UInteger::default(), size),
    }
}

fn length_encoded_vector<T, R: FnMut(&mut W, &T) -> WriteResult, W: std::io::Write>(
    out: &mut W,
    structures::LengthEncodedVector(items): &structures::LengthEncodedVector<T>,
    size: numeric::IntegerSize,
    mut writer: R,
) -> WriteResult {
    unsigned_length(out, items.len(), size)?;
    for e in items {
        writer(out, e)?;
    }
    Ok(())
}

fn length_encoded_indices<I: Into<numeric::UInteger> + Copy, W: std::io::Write>(
    out: &mut W,
    indices: &structures::LengthEncodedVector<I>,
    size: numeric::IntegerSize,
) -> WriteResult {
    length_encoded_vector(out, indices, size, |out, index| {
        unsigned_index(out, *index, size)
    })
}

fn version_numbers<W: std::io::Write>(
    out: &mut W,
    format::VersionNumbers(ref numbers): &format::VersionNumbers,
    size: numeric::IntegerSize,
) -> WriteResult {
    length_encoded_vector(out, numbers, size, |out, number| {
        unsigned_integer(out, *number, size)
    })
}

fn module_identifier<W: std::io::Write>(
    out: &mut W,
    id: &format::ModuleIdentifier,
    size: numeric::IntegerSize,
) -> WriteResult {
    identifier(out, &id.name, size)?;
    version_numbers(out, &id.version, size)
}

fn module_header<W: std::io::Write>(
    out: &mut W,
    header: &format::ModuleHeader,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_integer(out, header.field_count(), size)?;
    module_identifier(out, &header.identifier, size)
}

fn primitive_type<W: std::io::Write>(out: &mut W, t: type_system::PrimitiveType) -> WriteResult {
    write(out, type_system::TypeTagged::tag(&t) as u8)
}

fn simple_type<W: std::io::Write>(
    out: &mut W,
    t: &type_system::SimpleType,
    size: numeric::IntegerSize,
) -> WriteResult {
    write(out, type_system::TypeTagged::tag(t) as u8)?;
    match t {
        type_system::SimpleType::Primitive(_) => Ok(()),
        type_system::SimpleType::Defined(index) => unsigned_index(out, *index, size),
        type_system::SimpleType::NativePointer(pointee) => simple_type(out, pointee.as_ref(), size),
    }
}

fn heap_type<W: std::io::Write>(
    out: &mut W,
    t: &type_system::HeapType,
    size: numeric::IntegerSize,
) -> WriteResult {
    write(out, type_system::TypeTagged::tag(t) as u8)?;
    match t {
        type_system::HeapType::ObjRef(type_system::SimpleType::Defined(index)) => {
            unsigned_index(out, *index, size)
        }
        type_system::HeapType::ArrayRef(element_type)
        | type_system::HeapType::HeapPointer(element_type) => {
            heap_type(out, element_type.as_ref(), size)
        }
        type_system::HeapType::ObjRef(element_type) => simple_type(out, element_type, size),
        type_system::HeapType::Val(_) | type_system::HeapType::AnyRef => Ok(()),
    }
}

fn any_type<W: std::io::Write>(
    out: &mut W,
    t: &type_system::AnyType,
    size: numeric::IntegerSize,
) -> WriteResult {
    match t {
        type_system::AnyType::Heap(t) => heap_type(out, t, size),
        type_system::AnyType::GargbageCollectedPointer(element_type) => {
            write(out, type_system::TypeTag::GarbageCollectedPointer as u8)?;
            any_type(out, element_type.as_ref(), size)
        }
    }
}

fn method_signature<W: std::io::Write>(
    out: &mut W,
    signature: &format::MethodSignature,
    size: numeric::IntegerSize,
) -> WriteResult {
    length_encoded_indices(out, &signature.return_types, size)?;
    length_encoded_indices(out, &signature.parameter_types, size)
}

fn instruction_opcode<W: std::io::Write>(
    out: &mut W,
    opcode: instruction_set::Opcode,
) -> WriteResult {
    let mut value = opcode as usize;

    loop {
        write(out, value as u8)?;
        value -= std::cmp::min(value, instruction_set::Opcode::Continuation as usize);

        if value > 0 {
            continue;
        } else {
            return Ok(());
        }
    }
}

fn numeric_type<W: std::io::Write>(out: &mut W, t: instruction_set::NumericType) -> WriteResult {
    match t {
        instruction_set::NumericType::Primitive(pt) => primitive_type(out, pt),
    }
}

fn basic_arithmetic_operation<W: std::io::Write>(
    out: &mut W,
    operation: &instruction_set::BasicArithmeticOperation,
    size: numeric::IntegerSize,
) -> WriteResult {
    write(out, operation.flags().bits())?;
    numeric_type(out, operation.return_type)?;
    unsigned_index(out, operation.x, size)?;
    unsigned_index(out, operation.y, size)
}

fn division_operation<W: std::io::Write>(
    out: &mut W,
    operation: &instruction_set::DivisionOperation,
    size: numeric::IntegerSize,
) -> WriteResult {
    write(out, operation.flags().bits())?;
    numeric_type(out, operation.return_type)?;
    unsigned_index(out, operation.numerator, size)?;
    unsigned_index(out, operation.denominator, size)
}

fn block_instruction<W: std::io::Write>(
    out: &mut W,
    instruction: &instruction_set::Instruction,
    size: numeric::IntegerSize,
) -> WriteResult {
    use instruction_set::{Instruction, IntegerConstant, PrimitiveType};

    instruction_opcode(out, instruction.opcode())?;

    match instruction {
        Instruction::Nop => Ok(()),
        Instruction::Ret(registers) => length_encoded_indices(out, registers, size),
        Instruction::Add(operation) | Instruction::Sub(operation) | Instruction::Mul(operation) => {
            basic_arithmetic_operation(out, operation, size)
        }
        Instruction::Div(operation) => division_operation(out, operation, size),
        Instruction::ConstI(constant) => match constant {
            IntegerConstant::S8(value) => {
                primitive_type(out, PrimitiveType::S8)?;
                write(out, *value as u8)
            }
            IntegerConstant::U8(value) => {
                primitive_type(out, PrimitiveType::U8)?;
                write(out, *value)
            }
            IntegerConstant::S16(value) => {
                primitive_type(out, PrimitiveType::S16)?;
                write_bytes(out, &value.to_le_bytes())
            }
            IntegerConstant::U16(value) => {
                primitive_type(out, PrimitiveType::U16)?;
                write_bytes(out, &value.to_le_bytes())
            }
            IntegerConstant::S32(value) => {
                primitive_type(out, PrimitiveType::S32)?;
                write_bytes(out, &value.to_le_bytes())
            }
            IntegerConstant::U32(value) => {
                primitive_type(out, PrimitiveType::U32)?;
                write_bytes(out, &value.to_le_bytes())
            }
            IntegerConstant::S64(value) => {
                primitive_type(out, PrimitiveType::S64)?;
                write_bytes(out, &value.to_le_bytes())
            }
            IntegerConstant::U64(value) => {
                primitive_type(out, PrimitiveType::U64)?;
                write_bytes(out, &value.to_le_bytes())
            }
        },
        #[allow(unreachable_patterns)]
        _ => todo!("TODO: Add support for writing of more instructions"),
    }
}

fn code_block<W: std::io::Write>(
    out: &mut W,
    block: &format::CodeBlock,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> WriteResult {
    write(out, block.flags().bits())?;
    unsigned_integer(out, block.input_register_count, size)?;

    // Flags already indicate if exception handler is present or if exception register is used.
    if let Some(exception_handler) = &block.exception_handler {
        block_offset(out, exception_handler.catch_block, size)?;
        if let Some(exception_register) = exception_handler.exception_register {
            unsigned_index(out, exception_register, size)?;
        }
    }

    double_length_encoded_vector(
        out,
        block.instructions.as_ref(),
        size,
        buffer_pool,
        |out, instruction| block_instruction(out, instruction, size),
    )
}

fn method_body<W: std::io::Write>(
    out: &mut W,
    code: &format::Code,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> WriteResult {
    code_block(out, &code.entry_block, size, buffer_pool)?;
    length_encoded_vector(out, &code.blocks, size, |out, block| {
        code_block(out, block, size, buffer_pool)
    })
}

fn data_array<W: std::io::Write>(
    out: &mut W,
    format::DataArray(structures::LengthEncodedVector(bytes)): &format::DataArray,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn type_import<W: std::io::Write>(
    out: &mut W,
    import: &format::TypeImport,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, import.module, size)?;
    unsigned_index(out, import.name, size)?;
    unsigned_index(out, import.namespace, size)
}

fn field_import<W: std::io::Write>(
    out: &mut W,
    import: &format::FieldImport,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, import.owner, size)?;
    unsigned_index(out, import.name, size)?;
    unsigned_index(out, import.signature, size)
}

fn method_import<W: std::io::Write>(
    out: &mut W,
    import: &format::MethodImport,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, import.owner, size)?;
    unsigned_index(out, import.name, size)?;
    unsigned_index(out, import.signature, size)
}

fn double_length_encoded_vector<T, R: FnMut(&mut Vec<u8>, &T) -> WriteResult, W: std::io::Write>(
    out: &mut W,
    items: structures::ByteLengthEncoded<&structures::LengthEncodedVector<T>>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> WriteResult {
    byte_length_encoded(out, items, size, buffer_pool, |out, v, _| {
        length_encoded_vector(out, v, size, writer)
    })
}

fn module_imports<W: std::io::Write>(
    out: &mut W,
    imports: &format::ModuleImports,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> WriteResult {
    double_length_encoded_vector(
        out,
        imports.imported_modules.as_ref(),
        size,
        buffer_pool,
        |out, id| module_identifier(out, id, size),
    )?;
    double_length_encoded_vector(
        out,
        imports.imported_types.as_ref(),
        size,
        buffer_pool,
        |out, import| type_import(out, import, size),
    )?;
    double_length_encoded_vector(
        out,
        imports.imported_fields.as_ref(),
        size,
        buffer_pool,
        |out, import| field_import(out, import, size),
    )?;
    double_length_encoded_vector(
        out,
        imports.imported_methods.as_ref(),
        size,
        buffer_pool,
        |out, import| method_import(out, import, size),
    )
}

fn method_override<W: std::io::Write>(
    out: &mut W,
    entry: &format::MethodOverride,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, entry.declaration, size)?;
    unsigned_index(out, entry.implementation, size)
}

fn type_definition<W: std::io::Write>(
    out: &mut W,
    definition: &format::Type,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, definition.name, size)?;
    unsigned_index(out, definition.namespace, size)?;
    write(out, definition.visibility as u8)?;
    write(out, definition.flags.bits())?;
    unsigned_index(out, definition.layout, size)?;
    length_encoded_indices(out, &definition.inherited_types, size)?;
    length_encoded_indices(out, &definition.fields, size)?;
    length_encoded_indices(out, &definition.methods, size)?;
    length_encoded_vector(out, &definition.vtable, size, |out, entry| {
        method_override(out, entry, size)
    })
}

fn field_definition<W: std::io::Write>(
    out: &mut W,
    definition: &format::Field,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, definition.owner, size)?;
    unsigned_index(out, definition.name, size)?;
    write(out, definition.visibility as u8)?;
    write(out, definition.flags.bits())?;
    unsigned_index(out, definition.signature, size)
}

fn method_definition<W: std::io::Write>(
    out: &mut W,
    definition: &format::Method,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, definition.owner, size)?;
    unsigned_index(out, definition.name, size)?;
    write(out, definition.visibility as u8)?;
    write(out, definition.flags.bits())?;
    write(out, definition.implementation_flags().bits())?;
    unsigned_index(out, definition.signature, size)?;

    match definition.body {
        format::MethodBody::Defined(body) => unsigned_index(out, body, size),
        format::MethodBody::Abstract => Ok(()),
        format::MethodBody::External {
            library,
            entry_point_name,
        } => {
            unsigned_index(out, library, size)?;
            unsigned_index(out, entry_point_name, size)
        }
    }
}

fn module_definitions<W: std::io::Write>(
    out: &mut W,
    definitions: &format::ModuleDefinitions,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> WriteResult {
    double_length_encoded_vector(
        out,
        definitions.defined_types.as_ref(),
        size,
        buffer_pool,
        |out, definition| type_definition(out, definition, size),
    )?;
    double_length_encoded_vector(
        out,
        definitions.defined_fields.as_ref(),
        size,
        buffer_pool,
        |out, definition| field_definition(out, definition, size),
    )?;
    double_length_encoded_vector(
        out,
        definitions.defined_methods.as_ref(),
        size,
        buffer_pool,
        |out, definition| method_definition(out, definition, size),
    )
}

fn field_offset<W: std::io::Write>(
    out: &mut W,
    offset: &format::FieldOffset,
    size: numeric::IntegerSize,
) -> WriteResult {
    unsigned_index(out, offset.field, size)?;
    unsigned_index(out, offset.offset, size)
}

fn type_layout<W: std::io::Write>(
    out: &mut W,
    layout: &format::TypeLayout,
    size: numeric::IntegerSize,
) -> WriteResult {
    write(out, layout.flags() as u8)?;
    match layout {
        format::TypeLayout::Unspecified | format::TypeLayout::Sequential(None) => Ok(()),
        format::TypeLayout::Sequential(Some(type_size)) => unsigned_index(out, *type_size, size),
        format::TypeLayout::Explicit {
            size: type_size,
            field_offsets,
        } => {
            unsigned_index(out, *type_size, size)?;
            unimplemented!("Shouldn't the vector of field offsets be length encoded?")
        }
    }
}

/// Writes a binary module.
pub fn write_module<W: std::io::Write>(module: &format::Module, out: &mut W) -> WriteResult {
    write_bytes(out, format::MAGIC)?;
    write(out, module.integer_size as u8)?;
    format_version(out, &module.format_version, module.integer_size)?;
    unsigned_integer(out, module.data_count(), module.integer_size)?;

    let buffers = buffers::BufferPool::new();

    byte_length_encoded(
        out,
        module.header.as_ref(),
        module.integer_size,
        &buffers,
        |out, header, _| module_header(out, header, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.identifiers.as_ref(),
        module.integer_size,
        &buffers,
        |out, id| identifier(out, id, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.namespaces.as_ref(),
        module.integer_size,
        &buffers,
        |out, indices| length_encoded_indices(out, indices, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.type_signatures.as_ref(),
        module.integer_size,
        &buffers,
        |out, signature| any_type(out, signature, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.method_signatures.as_ref(),
        module.integer_size,
        &buffers,
        |out, signature| method_signature(out, signature, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.method_bodies.as_ref(),
        module.integer_size,
        &buffers,
        |out, code| method_body(out, code, module.integer_size, &buffers),
    )?;

    double_length_encoded_vector(
        out,
        module.data_arrays.as_ref(),
        module.integer_size,
        &buffers,
        |out, data| data_array(out, data, module.integer_size),
    )?;

    byte_length_encoded(
        out,
        module.imports.as_ref(),
        module.integer_size,
        &buffers,
        |out, imports, buffers| module_imports(out, imports, module.integer_size, buffers),
    )?;

    byte_length_encoded(
        out,
        module.definitions.as_ref(),
        module.integer_size,
        &buffers,
        |out, definitions, buffers| {
            module_definitions(out, definitions, module.integer_size, buffers)
        },
    )?;

    byte_length_optional(
        out,
        module.entry_point.as_ref(),
        module.integer_size,
        &buffers,
        |out, main_index, _| unsigned_index(out, *main_index, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.type_layouts.as_ref(),
        module.integer_size,
        &buffers,
        |out, layout| type_layout(out, layout, module.integer_size),
    )?;

    Ok(())
}
