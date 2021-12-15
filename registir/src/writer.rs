use crate::format as format;

#[non_exhaustive]
pub enum WriteError {
    VectorTooLarge(usize),
    IoError(std::io::Error),
}

pub type WriteResult = Result<(), WriteError>;

fn write_bytes<Destination: std::io::Write>(destination: &mut Destination, bytes: &[u8]) -> WriteResult {
    match destination.write_all(bytes) {
        Ok(()) => Ok(()),
        Err(err) => Err(WriteError::IoError(err)),
    }
}

trait BinWrite {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult;
}

impl BinWrite for format::uvarint {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::uvarint(value) = *self;
        let bytes = value.to_le_bytes();

        write_bytes(
            destination,
            if value < 0x80u64 {
                &bytes[0 .. 0]
            }
            else {
                panic!("Writing of integer {} is not supported", value)
            }
        )
    }
}

impl<T: format::Index> BinWrite for T {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.index().write(destination)
    }
}

fn vector_length<T>(v: &Vec<T>) -> Result<format::uvarint, WriteError> {
    let length = v.len();
    match u64::try_from(length) {
        Ok(length) => Ok(format::uvarint(length)),
        Err(_) => Err(WriteError::VectorTooLarge(length))
    }
}

impl BinWrite for format::Identifier {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let bytes = self.bytes();
        vector_length(bytes)?.write(destination)?;
        write_bytes(destination,bytes)
    }
}

impl BinWrite for format::FormatVersion {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.major.write(destination)?;
        self.minor.write(destination)
    }
}

impl<D: BinWrite> BinWrite for format::ByteLengthEncoded<D> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let mut buffer: Vec<u8> = Vec::new();
        let format::ByteLengthEncoded(data) = self;
        data.write(&mut buffer)?;
        vector_length(&buffer)?.write(destination)?;
        write_bytes(destination, buffer.as_slice())
    }
}

impl<T: BinWrite> BinWrite for format::LengthEncodedVector<T> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::LengthEncodedVector(items) = self;
        vector_length(items)?.write(destination)?;
        for e in items {
            e.write(destination)?;
        }
        Ok(())
    }
}

impl<D: BinWrite> BinWrite for format::ModuleData<D> {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        match self {
            Some(value) => value.write(destination),
            None => format::uvarint::default().write(destination)
        }
    }
}

impl BinWrite for format::VersionNumbers {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        let format::VersionNumbers(numbers) = self;
        numbers.write(destination)
    }
}

impl BinWrite for format::ModuleIdentifier {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.name.write(destination)?;
        self.version.write(destination)
    }
}

impl BinWrite for format::ModuleHeader {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        self.field_count().write(destination)?;
        self.identifier.write(destination)
    }
}

pub fn write<Destination: std::io::Write>(module: &format::Module, destination: &mut Destination) -> WriteResult {
    write_bytes(destination, format::MAGIC)?;
    module.format_version.write(destination)?;
    module.data_count().write(destination)?;
    module.header.write(destination)?;
    module.identifiers.write(destination)?;
    module.namespaces.write(destination)?;
    unimplemented!()
}
