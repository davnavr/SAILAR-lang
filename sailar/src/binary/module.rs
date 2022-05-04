//! Low-level API containing types that represent the contents of a SAILAR module binary.

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
    ModuleImport = 7,
    FunctionImport = 8,
    StructureImport = 9,
    GlobalImport = 10,
    FunctionDefinition = 11,
    StructureDefinition = 12,
    GlobalDefinition = 13,
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
        if value <= 16 {
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

mod writer {
    use crate::binary::VarIntSize;
    use std::io::{Error, ErrorKind, Write};

    pub type Result = std::io::Result<()>;

    pub type IntegerWriter<W> = fn(&mut Writer<W>, usize) -> Result;

    macro_rules! integer_writer {
        ($integer_type: ty) => {
            |out: &mut Writer<W>, value: usize| match <$integer_type>::try_from(value) {
                Ok(value) => out.write_all(&value.to_le_bytes()),
                Err(err) => Err(Error::new(ErrorKind::InvalidInput, err)),
            }
        };
    }

    fn select_integer_writer<W: Write>(integer_size: VarIntSize) -> IntegerWriter<W> {
        match integer_size {
            VarIntSize::One => integer_writer!(u8),
            VarIntSize::Two => integer_writer!(u16),
            VarIntSize::Four => integer_writer!(u32),
        }
    }

    pub struct Writer<W> {
        destination: W,
        integer_size: VarIntSize,
        integer_writer: IntegerWriter<W>,
    }

    pub type VecWriter<'a> = Writer<&'a mut Vec<u8>>;

    impl<W: Write> Writer<W> {
        pub fn new(destination: W, integer_size: VarIntSize) -> Self {
            Self {
                destination,
                integer_size,
                integer_writer: select_integer_writer(integer_size),
            }
        }

        #[inline]
        pub fn write_integer(&mut self, value: usize) -> Result {
            (self.integer_writer)(self, value)
        }

        pub fn write_identifier(&mut self, identifier: &crate::Id) -> Result {
            let bytes = identifier.as_bytes();
            self.write_integer(bytes.len())?;
            self.write_all(bytes)
        }

        pub fn derive_from<O: Write>(&self, other: O) -> Writer<O> {
            Writer {
                destination: other,
                integer_size: self.integer_size,
                integer_writer: select_integer_writer(self.integer_size),
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

    impl<W: std::fmt::Debug> std::fmt::Debug for Writer<W> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("Writer")
                .field("destination", &self.destination)
                .field("integer_size", &self.integer_size)
                .finish()
        }
    }
}

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

        let mut content_buffer = Vec::with_capacity(256);

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
