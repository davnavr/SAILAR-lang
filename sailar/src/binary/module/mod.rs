//! Low-level API containing types that represent the contents of a SAILAR module binary.

use crate::binary;
use crate::binary::record::{self, Record};
use crate::binary::signature;
use crate::versioning;
use std::io::{Read, Write};

mod writer;

pub mod reader;

/// Represents the content of a SAILAR module.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    format_version: versioning::Format,
    integer_size: binary::VarIntSize,
    records: Vec<Record<'a>>,
    //records: Vec<Vec<u8>>,
}

impl<'a> Module<'a> {
    pub fn new() -> Self {
        Self {
            format_version: versioning::Format::CURRENT.clone(),
            integer_size: binary::VarIntSize::One,
            records: Vec::default(),
        }
    }

    pub fn add_record<R: Into<Record<'a>>>(&mut self, record: R) {
        let record = record.into();
        // TODO: Conversion of record to bytes might be needed, problem is that during writing, large enough record could have byte length that exceeds max varinteger.
        self.records.push(record);
        self.integer_size.resize_to_fit(self.records.len());
    }

    #[inline]
    pub fn records(&self) -> &[record::Record<'a>] {
        &self.records
    }

    /// Writes the binary contents of the SAILAR module to the specified destination.
    pub fn write_to<W: Write>(&self, destination: W) -> std::io::Result<()> {
        use writer::{Result, VecWriter, Writer};

        let mut wrapper = Writer::new(destination, self.integer_size);
        let out = &mut wrapper;

        out.write_all(crate::binary::MAGIC)?;
        out.write_all(&[self.format_version.major, self.format_version.minor, self.integer_size.into()])?;
        out.write_integer(self.records.len())?;

        fn write_record_content(out: &mut VecWriter, record: &Record) -> Result {
            fn write_header_field(out: &mut VecWriter, field: &record::HeaderField) -> Result {
                out.write_identifier(field.field_name())?;
                match field {
                    record::HeaderField::ModuleIdentifier { name, version } => {
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
                    | Type::UPtr
                    | Type::SPtr
                    | Type::F32
                    | Type::F64
                    | Type::RawPtr(None) => Ok(()),
                    Type::RawPtr(Some(index)) => out.write_integer(*index),
                    Type::FuncPtr(index) => out.write_integer(*index),
                }
            }

            fn write_function_signature(out: &mut VecWriter, signature: &signature::Function) -> Result {
                let return_types = signature.return_types();
                let parameter_types = signature.parameter_types();
                out.write_integer(return_types.len())?;
                out.write_integer(parameter_types.len())?;
                for index in return_types.iter().chain(parameter_types) {
                    out.write_integer(*index)?;
                }
                Ok(())
            }

            // macro_rules! write_array_record {
            //     ($items: expr, $item_writer: expr) => {{
            //         out.write_integer($items.len())?;
            //         for item in $items.iter() {
            //             ($item_writer)(out, item)?;
            //         }
            //         Ok(())
            //     }};
            // }

            match record {
                Record::HeaderField(field) => write_header_field(out, field),
                Record::Identifier(identifier) => out.write_all(identifier.as_bytes()),
                Record::TypeSignature(signature) => write_type_signature(out, signature),
                Record::FunctionSignature(signature) => write_function_signature(out, signature),
                Record::Data(bytes) => out.write_all(bytes),
                // Record::Array(array) => {
                //     out.write_all(&[u8::from(array.item_type())])?;
                //     match array {
                //         record::Array::HeaderField(fields) => write_array_record!(fields, write_header_field),
                //         record::Array::Identifier(identifiers) => write_array_record!(identifiers, Writer::write_identifier),
                //         record::Array::TypeSignature(signatures) => write_array_record!(signatures, write_type_signature),
                //         record::Array::FunctionSignature(signatures) => write_array_record!(signatures, write_function_signature),
                //     }
                // }
            }
        }

        let mut content_buffer = Vec::with_capacity(64);

        for record in self.records.iter() {
            content_buffer.clear();
            let mut record_content = out.derive_from(&mut content_buffer);
            write_record_content(&mut record_content, &record)?;

            out.write_all(&[u8::from(record.record_type())])?;
            out.write_integer(content_buffer.len())?;
            out.write_all(&content_buffer)?;
        }

        Ok(())
    }
}

impl Module<'static> {
    pub fn from_reader<R: Read>(source: reader::Reader<R>) -> reader::Result<Self> {
        let (format_version, integer_size, mut reader) = source.to_record_reader()?;
        let mut records = Vec::with_capacity(reader.record_count());

        while let Some(record) = reader.next_record() {
            records.push(record?);
        }

        Ok(Self {
            format_version,
            integer_size,
            records,
        })
    }

    pub fn read_from<R: Read>(source: R) -> reader::Result<Self> {
        Self::from_reader(reader::Reader::new(source))
    }
}
