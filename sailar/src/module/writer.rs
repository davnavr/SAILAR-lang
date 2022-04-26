//! Code for emitting SAILAR modules.

use crate::binary::{self, buffer};
use crate::block;
use crate::function;
use crate::identifier::Id;
use crate::instruction_set;
use crate::type_system;
use std::io::Write;

type Result = std::io::Result<()>;

mod output {
    use super::Result;
    use crate::binary::VarIntSize;
    use crate::identifier::Id;
    use std::io::Write;

    type IntegerWriter<W> = fn(&mut Wrapper<W>, usize) -> Result;

    pub struct Wrapper<W> {
        destination: W,
        integer_writer: IntegerWriter<W>,
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

    length_writer!(integer_writer_one, u8);
    length_writer!(integer_writer_two, u16);
    length_writer!(integer_writer_four, u32);

    impl<W: Write> Wrapper<W> {
        pub fn new(destination: W, integer_size: VarIntSize) -> Self {
            Self {
                destination,
                integer_writer: match integer_size {
                    VarIntSize::One => Self::integer_writer_one,
                    VarIntSize::Two => Self::integer_writer_two,
                    VarIntSize::Four => Self::integer_writer_four,
                },
            }
        }

        pub fn write_integer(&mut self, length: usize) -> Result {
            (self.integer_writer)(self, length)
        }

        pub fn write_identifier(&mut self, identifier: &Id) -> Result {
            self.write_integer(identifier.len())?;
            self.destination.write_all(identifier.as_bytes())
        }

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

    impl<W: std::fmt::Debug> std::fmt::Debug for Wrapper<W> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("Wrapper").field("destination", &self.destination).finish()
        }
    }
}

mod lookup {
    use rustc_hash::FxHashMap;
    use std::collections::hash_map::Entry;

    // TODO: Checking the references instead of doing a slow Eq operation may be faster.
    //pub struct Key<'a, K>(&'a K);

    #[derive(Debug)]
    pub struct IndexMap<K> {
        lookup: FxHashMap<K, usize>,
    }

    impl<K: Eq + std::hash::Hash> IndexMap<K> {
        pub fn get_or_insert(&mut self, key: K) -> usize {
            let next_index = self.lookup.len();
            match self.lookup.entry(key) {
                Entry::Occupied(occupied) => *occupied.get(),
                Entry::Vacant(vacant) => *vacant.insert(next_index),
            }
        }

        pub fn len(&self) -> usize {
            self.lookup.len()
        }

        pub fn into_keys(self) -> impl std::iter::ExactSizeIterator<Item = K> {
            self.lookup.into_keys()
        }
    }

    impl<K> Default for IndexMap<K> {
        fn default() -> Self {
            Self {
                lookup: FxHashMap::default(),
            }
        }
    }
}

pub fn write<W: Write>(module: &crate::module::Definition, destination: W, buffer_pool: Option<&buffer::Pool>) -> Result {
    use output::Wrapper;

    let integer_size = module.integer_size;
    let mut out = Wrapper::new(destination, integer_size);
    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

    {
        out.write_all(binary::MAGIC.as_slice())?;
        let format_version = &module.format_version;
        out.write_all(&[format_version.major, format_version.minor, integer_size.into()])?;
        out.write_identifier(module.identifier().name())?;
        out.write_integer(module.identifier().version.len() * usize::from(integer_size.byte_count()))?;
        out.write_many(module.identifier().version.iter(), |numbers, version| {
            numbers.write_integer(*version)
        })?;
    }

    macro_rules! wrap_rented_buffer {
        ($buffer: expr) => {
            Wrapper::new($buffer.as_mut_vec(), integer_size)
        };
    }

    macro_rules! rent_default_buffer_wrapped {
        ($buffer_name: ident, $wrapper_name: ident) => {
            let mut $buffer_name = buffer_pool.rent();
            #[allow(unused_mut)]
            let mut $wrapper_name = wrap_rented_buffer!($buffer_name);
        };
    }
    
    let mut identifier_lookup = lookup::IndexMap::<&Id>::default();

    out.flush()
}
