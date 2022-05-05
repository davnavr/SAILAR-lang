//! Functions that provide a low-level way to read SAILAR modules, delegating to [`sailar::binary::module::reader`].

use crate::error::Error;
use sailar::binary::module::reader;

enum ReaderChoice {
    Buffer(reader::Reader<&'static [u8]>),
}

crate::box_wrapper!(ModuleReader, ReaderChoice, pub(self));

#[no_mangle]
pub unsafe extern "C" fn sailar_create_module_reader_from_buffer(buffer: *const u8, length: usize) -> ModuleReader {
    let source = if buffer.is_null() {
        &[]
    } else {
        std::slice::from_raw_parts(buffer, length)
    };

    ModuleReader::new(ReaderChoice::Buffer(reader::Reader::new(source)))
}

enum RecordReaderChoice {
    Buffer(reader::RecordReader<&'static [u8]>),
}

crate::box_wrapper!(RecordReader, RecordReaderChoice, pub(self));

#[no_mangle]
pub unsafe extern "C" fn sailar_get_records_from_module_reader(
    reader: ModuleReader,
    major_format_version: *mut u8,
    minor_format_version: *mut u8,
    integer_byte_size: *mut u8,
    error: *mut Error,
) -> RecordReader {
    match *reader.into_box() {
        ReaderChoice::Buffer(buffer_reader) => match buffer_reader.to_record_reader() {
            Ok((format_version, integer_size, reader)) => {
                *major_format_version = format_version.major;
                *minor_format_version = format_version.minor;
                *integer_byte_size = integer_size.byte_count();
                RecordReader::new(RecordReaderChoice::Buffer(reader))
            }
            Err(e) => {
                *error = Error::from_error(e);
                RecordReader::null()
            }
        },
    }
}
