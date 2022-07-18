//! Low-level API for building SAILAR binary modules.

use crate::binary::RawModule;
use crate::reader;
use crate::record::Record;
use crate::versioning;
use crate::writer;
use std::io::{Read, Write};

/// Allows writing the contents of a SAILAR module to a destination.
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

    /// Appends a record to this module.
    pub fn add_record<R: Into<Record<'a>>>(&mut self, record: R) {
        self.records.push(record.into());
    }

    /// Retrieves the records that are currently in this module.
    #[inline]
    pub fn records(&self) -> &[Record<'a>] {
        &self.records
    }

    /// Writes the binary contents of the SAILAR module to the specified destination.
    pub fn write_to<W: Write>(&self, destination: W) -> std::io::Result<()> {
        let mut wrapper = writer::Writer::new(destination);
        let out = &mut wrapper;

        out.write_all(crate::binary::MAGIC)?;
        out.write_all(&[self.format_version.major, self.format_version.minor])?;
        out.write_length(self.records.len())?;

        let mut content_buffer = Vec::with_capacity(64);

        for record in self.records.iter() {
            content_buffer.clear();
            let mut record_content = writer::VecWriter::new(&mut content_buffer);
            record_content.write_record_content(record)?;
            out.write_byte(u8::from(record.record_type()))?;
            out.write_length(content_buffer.len())?;
            out.write_all(&content_buffer)?;
        }

        Ok(())
    }

    /// Converts `self` into a vector containing the module's records.
    pub fn into_records(self) -> Vec<Record<'a>> {
        self.records
    }

    /// Retrieves the binary contents of the SAILAR module.
    ///
    /// # Example
    ///
    /// ```
    /// # use sailar::builder::Builder;
    /// let builder = Builder::new();
    /// // Insert code that adds records to the module here.
    /// let raw_contents = builder.to_raw_module();
    /// assert_eq!(sailar::binary::MAGIC.as_slice(), &raw_contents[0..6]);
    /// ```
    pub fn to_raw_module(&self) -> RawModule {
        unsafe {
            // Safety: Writer implementation is assumed to produce syntactically valid modules
            let mut contents = Vec::default();
            self.write_to(&mut contents).unwrap();
            RawModule::from_vec_unchecked(contents)
        }
    }
}

impl Builder<'static> {
    pub fn from_reader<R: Read>(source: reader::Reader<R>) -> reader::Result<Self> {
        let (format_version, mut reader) = source.to_record_reader()?;
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

impl Default for Builder<'_> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl From<&Builder<'_>> for RawModule {
    fn from(builder: &Builder<'_>) -> Self {
        builder.to_raw_module()
    }
}

impl From<Builder<'_>> for RawModule {
    fn from(builder: Builder<'_>) -> Self {
        builder.to_raw_module()
    }
}

impl<'a> Extend<Record<'a>> for Builder<'a> {
    fn extend<T: IntoIterator<Item = Record<'a>>>(&mut self, iter: T) {
        self.records.extend(iter)
    }
}
