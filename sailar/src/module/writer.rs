//! Code for emitting SAILAR modules.

use crate::binary::{self, buffer};

pub fn write<W: std::io::Write>(
    module: &crate::module::Module,
    destination: W,
    buffer_pool: Option<&buffer::Pool>,
) -> std::io::Result<()> {
    let mut out = destination;
    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);
    out.write_all(binary::MAGIC.as_slice())?;
    let format_version = &module.format_version;
    out.write_all(&[format_version.major, format_version.minor])?;
    todo!("create the raw contents");
    out.flush()
}
