//! Low-level internal API for writing the contents of a SAILAR binary module.

use crate::identifier::Id;
use crate::instruction::{self, Instruction};
use crate::num::VarU28;
use crate::record::{self, Record};
use crate::signature;
use std::io::{Error, ErrorKind, Write};

pub type Result = std::io::Result<()>;

#[derive(Debug)]
pub struct Writer<W> {
    destination: W,
}

pub type VecWriter<'a> = Writer<&'a mut Vec<u8>>;

impl<W: Write> Writer<W> {
    pub fn new(destination: W) -> Self {
        Self { destination }
    }

    pub fn write_byte(&mut self, value: u8) -> Result {
        self.destination.write_all(std::slice::from_ref(&value))
    }

    pub fn write_unsigned_integer<I: Into<VarU28>>(&mut self, value: I) -> Result {
        value.into().write_to(&mut self.destination)
    }

    pub fn write_length<I>(&mut self, value: I) -> Result
    where
        I: TryInto<VarU28>,
        I::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        match value.try_into() {
            Ok(value) => self.write_unsigned_integer(value),
            Err(err) => Err(Error::new(ErrorKind::InvalidInput, err)),
        }
    }

    pub fn write_identifier(&mut self, identifier: &Id) -> Result {
        let bytes = identifier.as_bytes();
        self.write_length(bytes.len())?;
        self.write_all(bytes)
    }

    fn write_export(&mut self, export: &record::Export) -> Result {
        self.write_unsigned_integer(export.flag_bits().map_err(|e| Error::new(ErrorKind::InvalidInput, e))?)?;
        if let Some(symbol) = export.symbol() {
            self.write_all(symbol.as_bytes())?;
        }
        Ok(())
    }

    fn write_metadata_field(&mut self, field: &record::MetadataField) -> Result {
        self.write_identifier(field.field_name())?;
        match field {
            record::MetadataField::ModuleIdentifier(identifier) => {
                self.write_identifier(identifier.name())?;
                self.write_length(identifier.version().len())?;
                for number in identifier.version().iter() {
                    self.write_unsigned_integer(*number)?;
                }
            }
            record::MetadataField::EntryPoint(entry) => self.write_length(*entry)?,
        }

        Ok(())
    }

    fn write_type_signature(&mut self, signature: &signature::Type) -> Result {
        match signature {
            signature::Type::FixedInteger(ty) => {
                if *ty == signature::IntegerType::U8 {
                    self.write_byte(signature::TypeCode::U8.into())
                } else if *ty == signature::IntegerType::S8 {
                    self.write_byte(signature::TypeCode::S8.into())
                } else if *ty == signature::IntegerType::U16 {
                    self.write_byte(signature::TypeCode::U16.into())
                } else if *ty == signature::IntegerType::S16 {
                    self.write_byte(signature::TypeCode::S16.into())
                } else if *ty == signature::IntegerType::U32 {
                    self.write_byte(signature::TypeCode::U32.into())
                } else if *ty == signature::IntegerType::S32 {
                    self.write_byte(signature::TypeCode::S32.into())
                } else if *ty == signature::IntegerType::U64 {
                    self.write_byte(signature::TypeCode::U64.into())
                } else if *ty == signature::IntegerType::S64 {
                    self.write_byte(signature::TypeCode::S64.into())
                } else if *ty == signature::IntegerType::U128 {
                    self.write_byte(signature::TypeCode::U128.into())
                } else if *ty == signature::IntegerType::S128 {
                    self.write_byte(signature::TypeCode::S128.into())
                } else if *ty == signature::IntegerType::U256 {
                    self.write_byte(signature::TypeCode::U256.into())
                } else if *ty == signature::IntegerType::S256 {
                    self.write_byte(signature::TypeCode::S256.into())
                } else {
                    let code = match ty.sign() {
                        signature::IntegerSign::Unsigned => signature::TypeCode::UInt,
                        signature::IntegerSign::Signed => signature::TypeCode::SInt,
                    };

                    self.write_byte(code.into())?;
                    self.write_length(ty.size())
                }
            }
            signature::Type::UAddr => self.write_byte(signature::TypeCode::UAddr.into()),
            signature::Type::SAddr => self.write_byte(signature::TypeCode::SAddr.into()),
            signature::Type::F32 => self.write_byte(signature::TypeCode::F32.into()),
            signature::Type::F64 => self.write_byte(signature::TypeCode::F64.into()),
            signature::Type::RawPtr(Some(index)) => {
                self.write_byte(signature::TypeCode::RawPtr.into())?;
                self.write_length(*index)
            }
            signature::Type::FuncPtr(index) => {
                self.write_byte(signature::TypeCode::FuncPtr.into())?;
                self.write_length(*index)
            }
            signature::Type::RawPtr(None) => self.write_byte(signature::TypeCode::VoidPtr.into()),
        }
    }

    fn write_function_signature(&mut self, signature: &signature::Function) -> Result {
        self.write_length(signature.return_type_len())?;
        self.write_length(signature.types().len() - signature.return_type_len())?;
        signature.types().iter().try_for_each(|index| self.write_length(*index))
    }

    fn write_code_value(&mut self, value: &instruction::Value) -> Result {
        let flags = value.flags();
        self.write_byte(flags.bits())?;
        match value {
            instruction::Value::IndexedRegister(index) => self.write_length(*index),
            instruction::Value::Constant(instruction::Constant::Integer(integer)) => match integer {
                _ if flags.contains(instruction::ValueFlags::INTEGER_IS_EMBEDDED) => Ok(()),
                instruction::ConstantInteger::I8(byte) => self.write_byte(*byte),
                instruction::ConstantInteger::I16(ref bytes) => self.write_all(bytes),
                instruction::ConstantInteger::I32(ref bytes) => self.write_all(bytes),
                instruction::ConstantInteger::I64(ref bytes) => self.write_all(bytes),
            },
        }
    }

    fn write_code_block(&mut self, block: &record::CodeBlock) -> Result {
        self.write_length(block.input_count)?;
        self.write_length(block.result_count)?;
        self.write_length(block.temporary_count())?;
        for index in block.register_types.iter() {
            self.write_length(*index)?;
        }

        self.write_length(block.instructions.len())?;
        for instruction in block.instructions.iter() {
            self.write_byte(u8::from(instruction.opcode()))?;
            match instruction {
                Instruction::Nop | Instruction::Break => (),
                Instruction::Ret(values) => {
                    self.write_length(values.len())?;
                    values.iter().try_for_each(|value| self.write_code_value(value))?;
                }
                Instruction::Call(callee, arguments) => {
                    self.write_length(*callee)?;
                    self.write_length(arguments.len())?;
                    arguments.iter().try_for_each(|argument| self.write_code_value(argument))?;
                }
                Instruction::AddI(operands) | Instruction::SubI(operands) => {
                    self.write_byte(u8::from(operands.overflow_behavior()))?;
                    self.write_code_value(operands.x_value())?;
                    self.write_code_value(operands.y_value())?;
                }
            }
        }

        Ok(())
    }

    fn write_function_template(&mut self, template: &record::FunctionTemplate) -> Result {
        self.write_export(&template.export)?;
        self.write_length(template.signature)?;
        self.write_length(template.entry_block)
    }

    fn write_function(&mut self, function: &record::Function) -> Result {
        self.write_length(function.template)?;
        self.write_unsigned_integer(0u8)
    }

    pub fn write_record_content(&mut self, record: &Record) -> Result {
        match record {
            Record::MetadataField(field) => self.write_metadata_field(field),
            Record::Identifier(identifier) => self.write_all(identifier.as_bytes()),
            Record::TypeSignature(signature) => self.write_type_signature(signature),
            Record::FunctionSignature(signature) => self.write_function_signature(signature),
            Record::Data(bytes) => self.write_all(bytes.as_ref()),
            Record::CodeBlock(block) => self.write_code_block(block),
            Record::FunctionTemplate(template) => self.write_function_template(template),
            Record::Function(function) => self.write_function(function),
        }
    }
}

impl<W> std::ops::Deref for Writer<W> {
    type Target = W;

    fn deref(&self) -> &W {
        &self.destination
    }
}

impl<W> std::ops::DerefMut for Writer<W> {
    fn deref_mut(&mut self) -> &mut W {
        &mut self.destination
    }
}
