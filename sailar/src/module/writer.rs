//! Code for emitting SAILAR modules.

use crate::binary::{self, buffer};
use std::io::Write;

type Result = std::io::Result<()>;

mod output {
    use super::Result;
    use crate::binary::LengthSize;
    use std::io::Write;

    type LengthIntegerWriter<W> = fn(&mut Wrapper<W>, usize) -> Result;

    pub struct Wrapper<W> {
        destination: W,
        length_writer: LengthIntegerWriter<W>,
    }

    macro_rules! length_writer {
        ($name: ident, $integer_type: ty) => {
            impl<W: Write> Wrapper<W> {
                fn $name(&mut self, length: usize) -> Result {
                    match <$integer_type>::try_from(length) {
                        Ok(value) => self.destination.write_all(&value.to_le_bytes()),
                        Err(_) => unreachable!(
                            "attempt to write invalid length value {}, but maximum was {}",
                            length,
                            <$integer_type>::MAX
                        ),
                    }
                }
            }
        };
    }

    length_writer!(length_writer_one, u8);
    length_writer!(length_writer_two, u16);
    length_writer!(length_writer_four, u32);

    impl<W: Write> Wrapper<W> {
        pub fn new(destination: W, length_size: LengthSize) -> Self {
            Self {
                destination,
                length_writer: match length_size {
                    LengthSize::One => Self::length_writer_one,
                    LengthSize::Two => Self::length_writer_two,
                    LengthSize::Four => Self::length_writer_four,
                },
            }
        }
    }

    impl<W> std::ops::Deref for Wrapper<W> {
        type Target = W;

        fn deref(&self) -> &W {
            &self.destination
        }
    }

    impl<W> std::ops::DerefMut for Wrapper<W> {
        fn deref_mut(&mut self) -> &mut W {
            &mut self.destination
        }
    }
}

pub fn write<W: Write>(
    module: &crate::module::Module,
    destination: W,
    buffer_pool: Option<&buffer::Pool>,
) -> Result {
    let mut out = output::Wrapper::new(destination, module.length_size());
    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

    {
        out.write_all(binary::MAGIC.as_slice())?;
        let format_version = &module.format_version;
        out.write_all(&[
            format_version.major,
            format_version.minor,
            module.length_size.into(),
        ])?;
    }

    {
        let mut header = buffer_pool.rent_with_capacity(32);
        let name = module.name.as_bytes();
        header.write_all(name)?;
        out.write_all(&header)?;
    }

    todo!("create the raw contents");
    out.flush()
}
