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

        #[must_use]
        pub fn write_length(&mut self, length: usize) -> Result {
            (self.length_writer)(self, length)
        }

        #[must_use]
        pub fn write_many<T, I: std::iter::IntoIterator<Item = T>, O: FnMut(&mut Self, T) -> Result>(
            &mut self,
            items: I,
            mut writer: O,
        ) -> Result {
            for item in items.into_iter() {
                writer(self, item)?;
            }
            Ok(())
        }

        #[must_use]
        pub fn write_size_and_count(&mut self, count: usize, buffer: &[u8]) -> Result {
            if count > 0 {
                self.write_length(buffer.len())?;
                self.write_length(count)?;
                self.destination.write_all(buffer)
            } else {
                self.write_length(0)
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

pub fn write<W: Write>(module: &crate::module::Module, destination: W, buffer_pool: Option<&buffer::Pool>) -> Result {
    use output::Wrapper;

    let length_size = module.length_size;
    let mut out = Wrapper::new(destination, length_size);
    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

    {
        out.write_all(binary::MAGIC.as_slice())?;
        let format_version = &module.format_version;
        out.write_all(&[format_version.major, format_version.minor, length_size.into()])?;
    }

    macro_rules! rent_default_buffer {
        () => {
            buffer_pool.rent_with_capacity(32)
        };
    }

    macro_rules! rent_wrapped_buffer {
        ($buffer_name: ident, $wrapper_name: ident) => {
            let mut $buffer_name = rent_default_buffer!();
            let mut $wrapper_name = Wrapper::new($buffer_name.as_mut_vec(), length_size);
        };
    }

    {
        rent_wrapped_buffer!(header_buffer, header);
        let name = module.name.as_bytes();
        header.write_length(name.len())?;
        header.write_all(name)?;
        header.write_length(module.version.len() * usize::from(length_size.byte_count()))?;
        header.write_many(module.version.iter(), |numbers, version| numbers.write_length(*version))?;

        out.write_length(header.len())?;
        out.write_all(&header)?;
    }

    let mut type_signatures = rent_default_buffer!();
    let mut function_signatures = rent_default_buffer!();
    let mut data = rent_default_buffer!();
    let mut code = rent_default_buffer!();
    let mut imports = rent_default_buffer!();
    let mut definitions_buffer = rent_default_buffer!();

    {
        let mut definitions = Wrapper::new(definitions_buffer.as_mut_vec(), length_size);

        {
            let function_definitions = module.function_definitions();
            rent_wrapped_buffer!(functions_buffer, functions);
            functions.write_many(function_definitions, |definitions, f| todo!())?;

            definitions.write_size_and_count(function_definitions.len(), &functions)?;
        }
    }

    out.flush()
}
