//! Low-level API containing types that represent the contents of a SAILAR module binary.

use crate::binary::signature;
use crate::binary::VarIntSize;
use crate::{FormatVersion, Id};
use std::io::Write;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum RecordType {
    HeaderField = 0,
    Array = 1,
    Identifier = 2,
    TypeSignature = 3,
    FunctionSignature = 4,
    Data = 5,
    Code = 6,
    //ModuleImport = 7,
    //FunctionImport = 8,
    //StructureImport = 9,
    //GlobalImport = 10,
    //FunctionDefinition = 11,
    //StructureDefinition = 12,
    //GlobalDefinition = 13,
    //FunctionInstantiation = 14,
    //StructureInstantiation = 15,
    //Namespace = 16,
    //ExceptionClassImport = 17,
    //ExceptionClassDefinition = 18,
    //AnnotationClassImport = 19,
    //AnnotationClassDefinition = 20,
    //DebuggingInformation = 21,
}

impl From<RecordType> for u8 {
    fn from(value: RecordType) -> u8 {
        value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid record type")]
pub struct InvalidRecordTypeError {
    value: u8,
}

impl TryFrom<u8> for RecordType {
    type Error = InvalidRecordTypeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= RecordType::Code.into() {
            Ok(unsafe { std::mem::transmute::<u8, Self>(value) })
        } else {
            Err(InvalidRecordTypeError { value })
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Array<'a> {
    HeaderField(Vec<HeaderField<'a>>),
    Identifier(Vec<&'a Id>),
    //TypeSignature(),
}

impl Array<'_> {
    pub fn item_type(&self) -> RecordType {
        match self {
            Self::HeaderField(_) => RecordType::HeaderField,
            Self::Identifier(_) => RecordType::Identifier,
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum HeaderField<'a> {
    ModuleIdentifier { name: &'a Id, version: &'a [usize] },
}

impl HeaderField<'_> {
    pub fn field_name(&self) -> &'static Id {
        let name = match self {
            Self::ModuleIdentifier { .. } => "mID",
        };

        unsafe { Id::from_str_unchecked(name) }
    }
}

// TODO: Array records only? Doesn't make sense to generate a whole entire record byte count and all just for one identifier, field, function, etc.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Record<'a> {
    HeaderField(HeaderField<'a>),
    Identifier(&'a Id),
    Array(Array<'a>),
}

impl Record<'_> {
    pub fn tag(&self) -> RecordType {
        match self {
            Self::HeaderField(_) => RecordType::HeaderField,
            Self::Identifier(_) => RecordType::Identifier,
            Self::Array(_) => RecordType::Array,
        }
    }
}

mod writer;

/// Represents the content of a SAILAR module.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    format_version: FormatVersion,
    integer_size: VarIntSize,
    records: Vec<Record<'a>>,
}

impl<'a> Module<'a> {
    pub fn new() -> Self {
        Self {
            format_version: FormatVersion::CURRENT.clone(),
            integer_size: VarIntSize::One,
            records: Vec::default(),
        }
    }

    pub fn add_record<R: Into<Record<'a>>>(&mut self, record: R) {
        self.records.push(record.into());
        self.integer_size.resize_to_fit(self.records.len());
    }

    #[inline]
    pub fn records(&self) -> &[Record<'a>] {
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
            fn write_header_field(out: &mut VecWriter, field: &HeaderField) -> Result {
                out.write_identifier(field.field_name())?;
                match field {
                    HeaderField::ModuleIdentifier { name, version } => {
                        out.write_identifier(name)?;
                        out.write_integer(version.len())?;
                        for number in version.iter() {
                            out.write_integer(*number)?;
                        }
                        Ok(())
                    }
                }
            }

            macro_rules! write_array_record {
                ($items: expr, $item_writer: expr) => {{
                    out.write_integer($items.len())?;
                    for item in $items.iter() {
                        ($item_writer)(out, item)?;
                    }
                    Ok(())
                }};
            }

            match record {
                Record::HeaderField(field) => write_header_field(out, field),
                Record::Identifier(identifier) => out.write_all(identifier.as_bytes()),
                Record::Array(array) => {
                    out.write_all(&[u8::from(array.item_type())])?;
                    match array {
                        Array::HeaderField(fields) => write_array_record!(fields, write_header_field),
                        Array::Identifier(identifiers) => write_array_record!(identifiers, Writer::write_identifier),
                    }
                }
            }
        }

        let mut content_buffer = Vec::with_capacity(64);

        for record in self.records.iter() {
            content_buffer.clear();
            let mut record_content = out.derive_from(&mut content_buffer);
            write_record_content(&mut record_content, &record)?;

            out.write_all(&[u8::from(record.tag())])?;
            out.write_integer(content_buffer.len())?;
            out.write_all(&content_buffer)?;
        }

        Ok(())
    }
}
