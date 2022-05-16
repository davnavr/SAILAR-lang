//! Functions that provide a low-level way to read SAILAR modules, delegating to [`sailar::binary::reader`].

use crate::buffer::Buffer;
use crate::error::{self, Error};
use crate::identifier::Identifier;
use crate::signature;
use sailar::binary::reader;
use sailar::binary::record;

struct ActualModuleFormat {
    format_version: sailar::versioning::SupportedFormat,
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

    fn check_finished(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let reader = std::mem::take(self);
        match reader {
            Self::Records(record_reader) => record_reader.finish().map_err(Box::from),
            Self::Module(_) => Err(Box::new(error::StaticError::from(
                "module reader has not reached end as format has not been parsed",
            ))),
            Self::Invalid => Err(Box::new(ReaderInvalidError)),
        }
    }
}

enum ReaderChoice {
    Buffer(ReaderState<&'static [u8]>),
    File(ReaderState<std::fs::File>),
}

crate::box_wrapper!(ModuleReader(pub(self) ReaderChoice));

#[no_mangle]
pub unsafe extern "C" fn sailar_create_module_reader_from_buffer(buffer: Buffer) -> ModuleReader {
    ModuleReader::new(ReaderChoice::Buffer(ReaderState::Module(reader::Reader::new(
        buffer.into_ref(),
    ))))
}

/// Creates a module reader from a UTF-8 string.
///
/// If the file is not found or the path is valid returns null along with an `error` value.
#[no_mangle]
pub unsafe extern "C" fn sailar_create_module_reader_from_path(
    path: *const u8,
    length: usize,
    error: *mut Error,
) -> ModuleReader {
    let path = crate::handle_error!(std::str::from_utf8(std::slice::from_raw_parts(path, length)), error);
    let file = crate::handle_error!(std::fs::File::create(path), error);
    ModuleReader::new(ReaderChoice::File(ReaderState::Module(reader::Reader::new(file))))
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_module_reader(reader: ModuleReader) {
    reader.into_box();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_read_module_format(reader: ModuleReader, error: *mut Error) -> ModuleFormat {
    let result = match reader.into_mut() {
        ReaderChoice::Buffer(buffer_reader) => buffer_reader.read_module_format(),
        ReaderChoice::File(file_reader) => file_reader.read_module_format(),
    };

    ModuleFormat::new(crate::handle_error!(result, error))
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

crate::box_wrapper!(Record(pub reader::Record));

#[no_mangle]
pub unsafe extern "C" fn sailar_read_module_next_record(reader: ModuleReader, error: *mut Error) -> Record {
    let result = match reader.into_mut() {
        ReaderChoice::Buffer(buffer_reader) => buffer_reader.read_next_record(),
        ReaderChoice::File(file_reader) => file_reader.read_next_record(),
    };

    match crate::handle_error!(result, error) {
        Some(record) => Record::new(record),
        None => Record::null(),
    }
}

/// Checks that no records remain in the module.
#[no_mangle]
pub unsafe extern "C" fn sailar_check_module_reader_finished(reader: ModuleReader, error: *mut Error) {
    let result = match reader.into_mut() {
        ReaderChoice::Buffer(buffer_reader) => buffer_reader.check_finished(),
        ReaderChoice::File(file_reader) => file_reader.check_finished(),
    };

    match result {
        Ok(()) => (),
        Err(e) => *error = Error::from_error(e),
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

/// Copies the contents of a record into a newly allocated identifier, or returns `null` if the record does not contain an
/// identifier.
///
/// The returned identifier should be freed with `sailar_dispose_identifier`.
#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_as_identifier(record: Record) -> Identifier {
    if let record::Record::Identifier(identifier) = record.into_mut() {
        Identifier::new(sailar::Identifier::from(std::convert::AsRef::as_ref(identifier)))
    } else {
        Identifier::null()
    }
}

/// Copies the contents of a record into a newly allocated type signature, or returns `null` if the record does not contain a
/// type signature.
///
/// The returned type signature should be freed with `sailar_dispose_type_signature`.
#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_as_type_signature(record: Record) -> signature::TypeSignature {
    if let record::Record::TypeSignature(signature) = record.into_mut() {
        signature::TypeSignature::new(signature.as_ref().clone())
    } else {
        signature::TypeSignature::null()
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_as_function_signature(record: Record) -> signature::FunctionSignature {
    if let record::Record::FunctionSignature(signature) = record.into_mut() {
        signature::FunctionSignature::new(signature.as_ref().clone())
    } else {
        signature::FunctionSignature::null()
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_module_record_as_data_buffer(record: Record) -> Buffer {
    if let record::Record::Data(bytes) = record.into_mut() {
        Buffer::new(Box::from(bytes.as_ref().as_bytes()))
    } else {
        Buffer::null()
    }
}
