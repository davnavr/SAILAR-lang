use crate::{
    buffers,
    format::{self, instruction_set, numeric, type_system},
};
use std::io::Write;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    VectorTooLarge(usize),
    InputOutputError(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VectorTooLarge(size) => write!(f, "{} is not a valid size for a vector", size),
            Self::InputOutputError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Self::InputOutputError(error)
    }
}

pub type Result = std::result::Result<(), Error>;

fn write_bytes<W: Write>(out: &mut W, bytes: &[u8]) -> Result {
    out.write_all(bytes)?;
    Ok(())
}

fn write<W: Write>(out: &mut W, value: u8) -> Result {
    write_bytes(out, &[value])
}

fn unsigned_integer<W: Write>(
    out: &mut W,
    numeric::UInteger(value): numeric::UInteger,
    size: numeric::IntegerSize,
) -> Result {
    write_bytes(out, &u32::to_le_bytes(value)[..(size.size() as usize)])
}

fn unsigned_index<W: Write, I: Into<numeric::UInteger>>(
    out: &mut W,
    index: I,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_integer(out, index.into(), size)
}

fn signed_integer<W: Write>(
    out: &mut W,
    numeric::SInteger(value): numeric::SInteger,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_integer(out, numeric::UInteger(value as u32), size)
}

fn unsigned_length<W: Write>(out: &mut W, length: usize, size: numeric::IntegerSize) -> Result {
    match u32::try_from(length) {
        Ok(value) => unsigned_integer(out, numeric::UInteger(value), size),
        Err(_) => Err(Error::VectorTooLarge(length)),
    }
}

fn identifier<W: Write>(
    out: &mut W,
    id: &format::Identifier,
    size: numeric::IntegerSize,
) -> Result {
    let bytes = id.as_bytes();
    debug_assert!(!bytes.is_empty());
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn format_version<W: Write>(
    out: &mut W,
    version: &format::FormatVersion,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_integer(out, version.major, size)?;
    unsigned_integer(out, version.minor, size)
}

fn byte_length_encoded<D, R: FnOnce(&mut Vec<u8>, &D, &buffers::BufferPool) -> Result, W: Write>(
    out: &mut W,
    format::LenBytes(data): format::LenBytes<&D>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> Result {
    let mut buffer = buffer_pool.rent();
    let bytes: &mut Vec<u8> = &mut buffer;
    writer(bytes, data, buffer_pool)?;
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn byte_length_optional<
    D,
    R: FnOnce(&mut Vec<u8>, &D, &buffers::BufferPool) -> Result,
    W: Write,
>(
    out: &mut W,
    format::LenBytes(ref wrapped): format::LenBytes<&Option<D>>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> Result {
    match wrapped {
        Some(data) => byte_length_encoded(out, format::LenBytes(data), size, buffer_pool, writer),
        None => unsigned_integer(out, numeric::UInteger::default(), size),
    }
}

fn length_encoded_vector<T, R: FnMut(&mut W, &T) -> Result, W: Write>(
    out: &mut W,
    format::LenVec(items): &format::LenVec<T>,
    size: numeric::IntegerSize,
    mut writer: R,
) -> Result {
    unsigned_length(out, items.len(), size)?;
    for e in items {
        writer(out, e)?;
    }
    Ok(())
}

fn length_encoded_indices<I: Into<numeric::UInteger> + Copy, W: Write>(
    out: &mut W,
    indices: &format::LenVec<I>,
    size: numeric::IntegerSize,
) -> Result {
    length_encoded_vector(out, indices, size, |out, index| {
        unsigned_index(out, *index, size)
    })
}

fn version_numbers<W: Write>(
    out: &mut W,
    format::VersionNumbers(ref numbers): &format::VersionNumbers,
    size: numeric::IntegerSize,
) -> Result {
    length_encoded_vector(out, numbers, size, |out, number| {
        unsigned_integer(out, *number, size)
    })
}

fn module_identifier<W: Write>(
    out: &mut W,
    id: &format::ModuleIdentifier,
    size: numeric::IntegerSize,
) -> Result {
    identifier(out, &id.name, size)?;
    version_numbers(out, &id.version, size)
}

fn module_header<W: Write>(
    out: &mut W,
    header: &format::ModuleHeader,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_integer(out, header.field_count(), size)?;
    module_identifier(out, &header.identifier, size)
}

fn primitive_type<W: Write>(out: &mut W, t: type_system::PrimitiveType) -> Result {
    write(out, type_system::TypeTagged::tag(&t) as u8)
}

fn simple_type<W: Write>(
    out: &mut W,
    t: &type_system::SimpleType,
    size: numeric::IntegerSize,
) -> Result {
    write(out, type_system::TypeTagged::tag(t) as u8)?;
    match t {
        type_system::SimpleType::Primitive(_) => Ok(()),
        type_system::SimpleType::Defined(index) => unsigned_index(out, *index, size),
        type_system::SimpleType::NativePointer(pointee) => simple_type(out, pointee.as_ref(), size),
    }
}

fn heap_type<W: Write>(
    out: &mut W,
    t: &type_system::HeapType,
    size: numeric::IntegerSize,
) -> Result {
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

fn any_type<W: Write>(out: &mut W, t: &type_system::AnyType, size: numeric::IntegerSize) -> Result {
    match t {
        type_system::AnyType::Heap(t) => heap_type(out, t, size),
        type_system::AnyType::GargbageCollectedPointer(element_type) => {
            write(out, type_system::TypeTag::GarbageCollectedPointer as u8)?;
            any_type(out, element_type.as_ref(), size)
        }
    }
}

fn function_signature<W: Write>(
    out: &mut W,
    signature: &format::FunctionSignature,
    size: numeric::IntegerSize,
) -> Result {
    length_encoded_indices(out, &signature.return_types, size)?;
    length_encoded_indices(out, &signature.parameter_types, size)
}

fn instruction_opcode<W: Write>(out: &mut W, opcode: instruction_set::Opcode) -> Result {
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

fn numeric_type<W: Write>(out: &mut W, t: instruction_set::NumericType) -> Result {
    match t {
        instruction_set::NumericType::Primitive(pt) => primitive_type(out, pt),
    }
}

fn basic_arithmetic_operation<W: Write>(
    out: &mut W,
    operation: &instruction_set::BasicArithmeticOperation,
    size: numeric::IntegerSize,
) -> Result {
    write(out, operation.flags().bits())?;
    unsigned_index(out, operation.x, size)?;
    unsigned_index(out, operation.y, size)
}

fn division_operation<W: Write>(
    out: &mut W,
    operation: &instruction_set::DivisionOperation,
    size: numeric::IntegerSize,
) -> Result {
    write(out, operation.flags().bits())?;
    if let instruction_set::DivideByZeroBehavior::Return(value_index) = operation.divide_by_zero {
        unsigned_index(out, value_index, size)?;
    }
    numeric_type(out, operation.return_type)?;
    unsigned_index(out, operation.numerator, size)?;
    unsigned_index(out, operation.denominator, size)
}

fn bitwise_operation<W: Write>(
    out: &mut W,
    operation: &instruction_set::BitwiseOperation,
    size: numeric::IntegerSize,
) -> Result {
    numeric_type(out, operation.result_type)?;
    unsigned_index(out, operation.x, size)?;
    unsigned_index(out, operation.y, size)
}

fn block_instruction<W: Write>(
    out: &mut W,
    instruction: &instruction_set::Instruction,
    size: numeric::IntegerSize,
) -> Result {
    use instruction_set::{Instruction, IntegerConstant, PrimitiveType};

    instruction_opcode(out, instruction.opcode())?;

    match instruction {
        Instruction::Nop | Instruction::Break => Ok(()),
        Instruction::Ret(registers) => length_encoded_indices(out, registers, size),
        // Instruction::Br(target, input_registers) => {
        //     unsigned_index(out, *target, size)?;
        //     length_encoded_indices(out, input_registers, size)
        // }
        // Instruction::BrIf {
        //     condition,
        //     true_branch,
        //     false_branch,
        //     input_registers,
        // } => {
        //     unsigned_index(out, *condition, size)?;
        //     unsigned_index(out, *true_branch, size)?;
        //     unsigned_index(out, *false_branch, size)?;
        //     length_encoded_indices(out, input_registers, size)
        // }
        Instruction::Call(call) => {
            unsigned_index(out, call.function, size)?;
            length_encoded_indices(out, &call.arguments, size)
        }
        Instruction::Add(operation) | Instruction::Sub(operation) | Instruction::Mul(operation) => {
            basic_arithmetic_operation(out, operation, size)
        }
        // Instruction::Div(operation) => division_operation(out, operation, size),
        // Instruction::And(operation)
        // | Instruction::Or(operation)
        // | Instruction::Xor(operation)
        // | Instruction::ShL(instruction_set::BitwiseShiftOperation(operation))
        // | Instruction::ShR(instruction_set::BitwiseShiftOperation(operation))
        // | Instruction::RotL(instruction_set::BitwiseShiftOperation(operation))
        // | Instruction::RotR(instruction_set::BitwiseShiftOperation(operation)) => {
        //     bitwise_operation(out, operation, size)
        // }
        // Instruction::Not(result_type, value) => {
        //     numeric_type(out, *result_type)?;
        //     unsigned_index(out, *value, size)
        // }
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
    }
}

fn code_block<W: Write>(
    out: &mut W,
    block: &format::CodeBlock,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result {
    write(out, block.flags().bits())?;
    unsigned_integer(out, block.input_register_count, size)?;

    // Flags already indicate if exception handler is present or if exception register is used.
    if let Some(exception_handler) = &block.exception_handler {
        unsigned_index(out, exception_handler.catch_block, size)?;
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

fn function_body<W: Write>(
    out: &mut W,
    code: &format::Code,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result {
    code_block(out, &code.entry_block, size, buffer_pool)?;
    length_encoded_vector(out, &code.blocks, size, |out, block| {
        code_block(out, block, size, buffer_pool)
    })
}

fn data_array<W: Write>(
    out: &mut W,
    format::DataArray(format::LenVec(bytes)): &format::DataArray,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_length(out, bytes.len(), size)?;
    write_bytes(out, bytes)
}

fn namespace_definition<W: Write>(
    out: &mut W,
    namespace: &format::Namespace,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, namespace.name, size)?;
    write(out, namespace.flags().bits())?;
    if let Some(parent) = namespace.parent {
        unsigned_index(out, parent, size)?;
    }
    length_encoded_indices(out, &namespace.structs, size)?;
    length_encoded_indices(out, &namespace.globals, size)?;
    length_encoded_indices(out, &namespace.functions, size)
}

fn struct_import<W: Write>(
    out: &mut W,
    import: &format::StructImport,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, import.module, size)?;
    unsigned_index(out, import.symbol, size)
}

fn global_import<W: Write>(
    out: &mut W,
    import: &format::GlobalImport,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, import.module, size)?;
    unsigned_index(out, import.symbol, size)?;
    unsigned_index(out, import.signature, size)
}

fn field_import<W: Write>(
    out: &mut W,
    import: &format::FieldImport,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, import.owner, size)?;
    unsigned_index(out, import.symbol, size)?;
    unsigned_index(out, import.signature, size)
}

fn function_import<W: Write>(
    out: &mut W,
    import: &format::FunctionImport,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, import.module, size)?;
    unsigned_index(out, import.symbol, size)?;
    unsigned_index(out, import.signature, size)
}

fn double_length_encoded_vector<T, R: FnMut(&mut Vec<u8>, &T) -> Result, W: Write>(
    out: &mut W,
    items: format::LenBytes<&format::LenVec<T>>,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    writer: R,
) -> Result {
    byte_length_encoded(out, items, size, buffer_pool, |out, v, _| {
        length_encoded_vector(out, v, size, writer)
    })
}

fn module_imports<W: Write>(
    out: &mut W,
    imports: &format::ModuleImports,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result {
    double_length_encoded_vector(
        out,
        imports.imported_modules.as_ref(),
        size,
        buffer_pool,
        |out, id| module_identifier(out, id, size),
    )?;
    double_length_encoded_vector(
        out,
        imports.imported_structs.as_ref(),
        size,
        buffer_pool,
        |out, import| struct_import(out, import, size),
    )?;
    double_length_encoded_vector(
        out,
        imports.imported_globals.as_ref(),
        size,
        buffer_pool,
        |out, import| global_import(out, import, size),
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
        imports.imported_functions.as_ref(),
        size,
        buffer_pool,
        |out, import| function_import(out, import, size),
    )
}

fn struct_definition<W: Write>(
    out: &mut W,
    definition: &format::Struct,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, definition.name, size)?;
    write(out, definition.flags().bits())?;
    unsigned_index(out, definition.symbol, size)?;
    unsigned_index(out, definition.layout, size)?;
    length_encoded_indices(out, &definition.fields, size)
}

fn global_definition<W: Write>(
    out: &mut W,
    definition: &format::Global,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, definition.name, size)?;
    write(out, definition.flags().bits())?;
    unsigned_index(out, definition.symbol, size)?;
    unsigned_index(out, definition.signature, size)
}

fn field_definition<W: Write>(
    out: &mut W,
    definition: &format::Field,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, definition.owner, size)?;
    unsigned_index(out, definition.name, size)?;
    write(out, definition.flags().bits())?;
    unsigned_index(out, definition.symbol, size)?;
    unsigned_index(out, definition.signature, size)
}

fn function_definition<W: Write>(
    out: &mut W,
    definition: &format::Function,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, definition.name, size)?;
    unsigned_index(out, definition.signature, size)?;
    write(out, definition.flags().bits())?;
    unsigned_index(out, definition.symbol, size)?;

    match definition.body {
        format::FunctionBody::Defined(body) => unsigned_index(out, body, size),
        format::FunctionBody::External {
            library,
            entry_point_name,
        } => {
            unsigned_index(out, library, size)?;
            unsigned_index(out, entry_point_name, size)
        }
    }
}

fn module_definitions<W: Write>(
    out: &mut W,
    definitions: &format::ModuleDefinitions,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result {
    double_length_encoded_vector(
        out,
        definitions.defined_structs.as_ref(),
        size,
        buffer_pool,
        |out, definition| struct_definition(out, definition, size),
    )?;
    double_length_encoded_vector(
        out,
        definitions.defined_globals.as_ref(),
        size,
        buffer_pool,
        |out, definition| global_definition(out, definition, size),
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
        definitions.defined_functions.as_ref(),
        size,
        buffer_pool,
        |out, definition| function_definition(out, definition, size),
    )
}

fn field_offset<W: Write>(
    out: &mut W,
    offset: &format::FieldOffset,
    size: numeric::IntegerSize,
) -> Result {
    unsigned_index(out, offset.field, size)?;
    unsigned_index(out, offset.offset, size)
}

fn struct_layout<W: Write>(
    out: &mut W,
    layout: &format::StructLayout,
    size: numeric::IntegerSize,
) -> Result {
    write(out, layout.flags() as u8)?;
    match layout {
        format::StructLayout::Unspecified | format::StructLayout::Sequential(None) => Ok(()),
        format::StructLayout::Sequential(Some(type_size)) => unsigned_index(out, *type_size, size),
        format::StructLayout::Explicit {
            size: type_size,
            field_offsets,
        } => {
            unsigned_index(out, *type_size, size)?;
            todo!("Shouldn't the vector of field offsets be length encoded?")
        }
    }
}

/// Writes a binary module.
pub fn write_module<W: Write>(module: &format::Module, out: &mut W) -> Result {
    write_bytes(out, format::MAGIC)?;
    write(out, module.integer_size as u8)?;
    format_version(out, &module.format_version, module.integer_size)?;
    unsigned_integer(out, format::MAX_MODULE_DATA_COUNT, module.integer_size)?;

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
        |out, namespace| namespace_definition(out, namespace, module.integer_size),
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
        module.function_signatures.as_ref(),
        module.integer_size,
        &buffers,
        |out, signature| function_signature(out, signature, module.integer_size),
    )?;

    double_length_encoded_vector(
        out,
        module.function_bodies.as_ref(),
        module.integer_size,
        &buffers,
        |out, code| function_body(out, code, module.integer_size, &buffers),
    )?;

    double_length_encoded_vector(
        out,
        module.data.as_ref(),
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

    double_length_encoded_vector(
        out,
        module.struct_layouts.as_ref(),
        module.integer_size,
        &buffers,
        |out, layout| struct_layout(out, layout, module.integer_size),
    )?;

    byte_length_optional(
        out,
        module.entry_point.as_ref(),
        module.integer_size,
        &buffers,
        |out, main_index, _| unsigned_index(out, *main_index, module.integer_size),
    )?;

    Ok(())
}
