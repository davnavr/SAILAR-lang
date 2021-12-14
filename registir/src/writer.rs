#[non_exhaustive]
pub enum WriteError {
    IoError(std::io::Error)
}

type WriteResult = Result<(), WriteError>;

fn map_io_result<T>(result: std::io::Result<T>) -> WriteResult {
    match result {
        Ok(_) => Ok(()),
        Err(err) => Err(WriteError::IoError(err)),
    }
}

trait BinWrite {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult;
}

impl BinWrite for &[u8] {
    fn write<Destination: std::io::Write>(&self, destination: &mut Destination) -> WriteResult {
        map_io_result(destination.write(self))
    }
}

fn write_bytes<T: BinWrite, Destination: std::io::Write>(writer: T, destination: &mut Destination) -> WriteResult {
    writer.write(destination)
}

pub fn write<Destination: std::io::Write>(module: crate::format::Module, destination: &mut Destination) -> Result<(), WriteError> {
    write_bytes(crate::format::MAGIC, destination)
}
