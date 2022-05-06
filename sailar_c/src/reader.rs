//! Functions that provide a low-level way to read SAILAR modules, delegating to [`sailar::binary::module::reader`].

use crate::buffer::Buffer;
use crate::error::{self, Error};
use crate::identifier::Identifier;
use sailar::binary::module::reader;
use sailar::binary::record;

struct ActualModuleFormat {
    format_version: sailar::versioning::Format,
    integer_byte_size: u8,
}

crate::box_wrapper!(ModuleFormat(pub(self) ActualModuleFormat));

#[derive(Debug, thiserror::Error)]
#[error("cannot parse module format as the reader is currently positioned at the module records")]
struct ReaderAlreadyParsedFormatError;

#[derive(Debug, thiserror::Error)]
#[error("error occured with this reader")]
struct ReaderInvalidError;

enum ReaderState<R> {
    Invalid,
    Module(reader::Reader<R>),
    Records(reader::RecordReader<R>),
}

impl<R> std::default::Default for ReaderState<R> {
    fn default() -> Self {
        Self::Invalid
    }
}

impl<R: std::io::Read> ReaderState<R> {
    fn read_module_format(&mut self) -> Result<ActualModuleFormat, Box<dyn std::error::Error>> {
        let reader = std::mem::take(self);
        match reader {
            Self::Module(module_reader) => {
                let (format_version, integer_size, record_reader) = module_reader.to_record_reader()?;
                *self = Self::Records(record_reader);
                Ok(ActualModuleFormat {
                    format_version,
                    integer_byte_size: integer_size.byte_count(),
                })
            }
            Self::Records(_) => Err(Box::new(ReaderAlreadyParsedFormatError)),
            Self::Invalid => Err(Box::new(ReaderInvalidError)),
        }
    }

    fn read_next_record(&mut self) -> Result<Option<record::Record<'static>>, Box<dyn std::error::Error>> {
        match self {
            Self::Records(record_reader) => record_reader.next_record().transpose().map_err(Box::from),
            Self::Module(_) => unreachable!("expected record reader but got module reader"),
            Self::Invalid => Err(Box::new(ReaderInvalidError)),
        }
    }
}

enum ReaderChoice {
    Buffer(ReaderState<&'static [u8]>),
    File(ReaderState<&'static std::fs::File>),
}

crate::box_wrapper!(ModuleReader(pub(self) ReaderChoice));

#[no_mangle]
pub unsafe extern "C" fn sailar_create_module_reader_from_buffer(buffer: Buffer) -> ModuleReader {
    ModuleReader::new(ReaderChoice::Buffer(ReaderState::Module(reader::Reader::new(
        buffer.into_ref(),
    ))))
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_module_reader(reader: ModuleReader) {
    reader.into_box();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_read_module_format(reader: ModuleReader, error: *mut Error) -> ModuleFormat {
    macro_rules! read {
        ($state: ident) => {
            error::handle_result($state.read_module_format(), error, |format| ModuleFormat::new(format))
        };
    }

    match reader.into_mut() {
        ReaderChoice::Buffer(buffer_reader) => read!(buffer_reader),
        ReaderChoice::File(file_reader) => read!(file_reader),
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_format_major_version(format: ModuleFormat) -> u8 {
    format.into_ref().format_version.major
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_format_minor_version(format: ModuleFormat) -> u8 {
    format.into_ref().format_version.minor
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_format_integer_byte_size(format: ModuleFormat) -> u8 {
    format.into_ref().integer_byte_size
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_module_format(format: ModuleFormat) {
    format.into_box();
}

crate::box_wrapper!(Record(pub record::Record<'static>));

#[no_mangle]
pub unsafe extern "C" fn sailar_read_module_next_record(reader: ModuleReader, error: *mut Error) -> Record {
    macro_rules! read {
        ($state: ident) => {
            error::handle_result($state.read_next_record(), error, |record| match record {
                Some(record) => Record::new(record),
                None => Record::null(),
            })
        };
    }

    match reader.into_mut() {
        ReaderChoice::Buffer(buffer_reader) => read!(buffer_reader),
        ReaderChoice::File(file_reader) => read!(file_reader),
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_module_record(record: Record) {
    record.into_box();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_type(record: Record) -> record::Type {
    record.into_mut().record_type()
}

/// Copies the contents of a record into a newly allocated identifier, or returns `null` if the record is not an identifier.
/// 
/// The returned identifier should be freeed with `sailar_dispose_identifier`.
#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_as_identifier(record: Record) -> Identifier {
    match record.into_mut() {
        record::Record::Identifier(identifier) => {
            Identifier::new(sailar::Identifier::from(std::convert::AsRef::as_ref(identifier)))
        }
        _ => Identifier::null(),
    }
}
