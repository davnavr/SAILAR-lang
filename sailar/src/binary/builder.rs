//! Low-level API for building SAILAR binary modules.

use crate::binary::reader;
use crate::binary::record::{self, Record};
use crate::binary::signature;
use crate::binary::writer;
use crate::versioning;
use std::io::{Read, Write};

/// Allows the writing the contents of a SAILAR module to a destination.
///
/// For simplicty and to avoid edge cases, modules produced by `Writer` always default to an integer size of 4 bytes.
#[derive(Clone, Debug)]
pub struct Builder<'a> {
    format_version: versioning::SupportedFormat,
    records: Vec<Record<'a>>,
}

impl<'a> Builder<'a> {
    pub fn with_format_version(format_version: versioning::SupportedFormat) -> Self {
        Self {
            format_version,
            records: Vec::default(),
        }
    }

    pub fn new() -> Self {
        Self::with_format_version(versioning::SupportedFormat::CURRENT)
    }

    #[inline]
    pub fn format_version(&self) -> &versioning::SupportedFormat {
        &self.format_version
    }

    pub fn add_record<R: Into<Record<'a>>>(&mut self, record: R) {
        let record = record.into();
        self.records.push(record);
    }

    #[inline]
    pub fn records(&self) -> &[record::Record<'a>] {
        &self.records
    }

    /// Writes the binary contents of the SAILAR module to the specified destination.
    pub fn write_to<W: Write>(&self, destination: W) -> std::io::Result<()> {
        use writer::{Result, VecWriter, Writer};

        let mut wrapper = Writer::new(destination);
        let out = &mut wrapper;

        out.write_all(crate::binary::MAGIC)?;

        out.write_all(&[
            self.format_version.major,
            self.format_version.minor,
            crate::binary::VarIntSize::Four.into(),
        ])?;

        out.write_integer(self.records.len())?;

        fn write_record_content(out: &mut VecWriter, record: &Record) -> Result {
            fn write_metadata_field(out: &mut VecWriter, field: &record::MetadataField) -> Result {
                out.write_identifier(field.field_name())?;
                match field {
                    record::MetadataField::ModuleIdentifier { name, version } => {
                        out.write_identifier(name.as_ref())?;
                        out.write_integer(version.len())?;
                        for number in version.iter() {
                            out.write_integer(*number)?;
                        }
                        Ok(())
                    }
                }
            }

            fn write_type_signature(out: &mut VecWriter, signature: &signature::Type) -> Result {
                use signature::Type;
                out.write_all(&[u8::from(signature.code())])?;
                match signature {
                    Type::U8
                    | Type::S8
                    | Type::U16
                    | Type::S16
                    | Type::U32
                    | Type::S32
                    | Type::U64
                    | Type::S64
                    | Type::UAddr
                    | Type::SAddr
                    | Type::F32
                    | Type::F64
                    | Type::RawPtr(None) => Ok(()),
                    Type::RawPtr(Some(index)) => out.write_integer(*index),
                    Type::FuncPtr(index) => out.write_integer(*index),
                }
            }

            fn write_function_signature(out: &mut VecWriter, signature: &signature::Function) -> Result {
                out.write_integer(signature.return_type_len())?;
                out.write_integer(signature.types().len() - signature.return_type_len())?;
                for index in signature.types().iter() {
                    out.write_integer(*index)?;
                }
                Ok(())
            }

            fn write_function_definition(out: &mut VecWriter, definition: &record::FunctionDefinition) -> Result {
                out.write_all(&[definition.flag_bits()])?;
                out.write_integer(0usize)?;
                out.write_integer(definition.signature())?;
                out.write_identifier(definition.symbol())?;

                match definition.body() {
                    record::FunctionBody::Definition(index) => out.write_integer(*index),
                    record::FunctionBody::Foreign { library, entry_point } => {
                        out.write_integer(*library)?;
                        out.write_identifier(entry_point)
                    }
                }
            }

            fn write_function_instantiation(out: &mut VecWriter, instantiation: &record::FunctionInstantiation) -> Result {
                out.write_integer(instantiation.template())?;
                out.write_integer(0usize)
            }

            match record {
                Record::MetadataField(field) => write_metadata_field(out, field),
                Record::Identifier(identifier) => out.write_all(identifier.as_bytes()),
                Record::TypeSignature(signature) => write_type_signature(out, signature),
                Record::FunctionSignature(signature) => write_function_signature(out, signature),
                Record::Data(bytes) => out.write_all(bytes.as_ref().as_bytes()),
                Record::CodeBlock(_) => todo!("write code"),
                Record::FunctionDefinition(definition) => write_function_definition(out, definition.as_ref()),
                Record::FunctionInstantiation(instantiation) => write_function_instantiation(out, instantiation.as_ref()),
            }
        }

        let mut content_buffer = Vec::with_capacity(64);

        for record in self.records.iter() {
            content_buffer.clear();
            let mut record_content = VecWriter::new(&mut content_buffer);
            write_record_content(&mut record_content, record)?;
            out.write_all(&[u8::from(record.record_type())])?;
            out.write_integer(content_buffer.len())?;
            out.write_all(&content_buffer)?;
        }

        Ok(())
    }
}

impl Default for Builder<'_> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Builder<'static> {
    pub fn from_reader<R: Read>(source: reader::Reader<R>) -> reader::Result<Self> {
        let (format_version, _, mut reader) = source.to_record_reader()?;
        let mut records = Vec::with_capacity(reader.record_count());

        while let Some(record) = reader.next_record() {
            records.push(record?);
        }

        Ok(Self { format_version, records })
    }

    pub fn read_from<R: Read>(source: R) -> reader::Result<Self> {
        Self::from_reader(reader::Reader::new(source))
    }
}
