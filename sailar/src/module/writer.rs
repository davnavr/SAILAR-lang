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
    use crate::binary::{self, VarIntSize};
    use crate::identifier::Id;
    use std::io::Write;

    type IntegerWriter<W> = fn(&mut Wrapper<W>, usize) -> Result;

    pub struct Wrapper<W> {
        destination: W,
        integer_size: VarIntSize,
        integer_writer: IntegerWriter<W>,
    }

    pub type BufferWrapper<'a> = Wrapper<&'a mut Vec<u8>>;

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
                integer_size,
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

        pub fn write_record(
            &mut self,
            tag: binary::RecordType,
            pool: &binary::buffer::Pool,
            writer: impl FnOnce(&mut BufferWrapper<'_>) -> Result,
        ) -> Result {
            let buffer = pool.rent();
            {
                let mut wrapper = Wrapper::new(buffer.as_mut_vec(), self.integer_size);
                writer(&mut wrapper)?;
            }
            self.destination.write_all(&[u8::from(tag)])?;
            self.write_integer(buffer.len())?;
            self.destination.write_all(&buffer)
        }

        pub fn write_record_array<
            T,
            I: std::iter::ExactSizeIterator<Item = T>,
            A: std::iter::IntoIterator<IntoIter = I>,
            O: FnMut(&mut BufferWrapper<'_>, T) -> Result,
        >(
            &mut self,
            tag: binary::RecordType,
            pool: &binary::buffer::Pool,
            items: A,
            writer: O,
        ) -> Result {
            self.write_record(binary::RecordType::Array, pool, |contents| {
                contents.destination.write_all(&[u8::from(tag)])?;
                let iterator = items.into_iter();
                contents.write_integer(iterator.len())?;
                contents.write_many(iterator, writer)
            })
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

    #[derive(Debug)]
    pub struct IndexMap<'a, K: ?Sized> {
        items: Vec<&'a K>,
        lookup: FxHashMap<&'a K, usize>,
    }

    impl<'a, K: Eq + std::hash::Hash + ?Sized> IndexMap<'a, K> {
        pub fn get_or_insert(&mut self, key: &'a K) -> usize {
            let next_index = self.items.len();
            match self.lookup.entry(key) {
                Entry::Occupied(occupied) => *occupied.get(),
                Entry::Vacant(vacant) => {
                    self.items.push(key);
                    *vacant.insert(next_index)
                }
            }
        }

        pub fn len(&self) -> usize {
            self.lookup.len()
        }

        pub fn into_iter(self) -> impl std::iter::ExactSizeIterator<Item = &'a K> {
            self.items.into_iter()
        }
    }

    impl<K: ?Sized> Default for IndexMap<'_, K> {
        fn default() -> Self {
            Self {
                items: Vec::default(),
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

    let mut identifier_lookup = lookup::IndexMap::<Id>::default();
    let mut code_block_lookup = lookup::IndexMap::<block::Block>::default();
    let mut function_signature_lookup = lookup::IndexMap::<function::Signature>::default();
    let mut type_signature_lookup = lookup::IndexMap::<type_system::Any>::default();

    out.write_record_array(
        binary::RecordType::FunctionDefinition,
        &buffer_pool,
        module.function_definitions(),
        |contents, definition| {
            contents.write_all(&[definition.definition().flags().bits()])?;
            contents.write_integer(function_signature_lookup.get_or_insert(definition.template().function().signature()))?;
            contents.write_identifier(definition.template().function().symbol())?;

            match definition.definition().body() {
                function::Body::Defined(defined) => contents.write_integer(code_block_lookup.get_or_insert(defined)),
                function::Body::Foreign(foreign) => {
                    contents.write_integer(identifier_lookup.get_or_insert(foreign.library_name().as_id()))?;
                    contents.write_identifier(foreign.entry_point_name())
                }
            }
        },
    )?;

    // TODO: Write code blocks.

    out.write_record_array(
        binary::RecordType::Identifier,
        &buffer_pool,
        identifier_lookup.into_iter(),
        |contents, identifier| contents.write_identifier(identifier),
    )?;

    out.write_record_array(
        binary::RecordType::FunctionSignature,
        &buffer_pool,
        function_signature_lookup.into_iter(),
        |contents, signature| {
            contents.write_integer(signature.result_types().len())?;
            contents.write_integer(signature.parameter_types().len())?;
            let all_types = signature.result_types().iter().chain(signature.parameter_types());
            contents.write_many(all_types, |types, function_type| {
                types.write_integer(type_signature_lookup.get_or_insert(function_type))
            })
        },
    )?;

    out.write_record_array(
        binary::RecordType::TypeSignature,
        &buffer_pool,
        type_signature_lookup.into_iter(),
        |contents, signature| {
            contents.write_all(&[u8::from(signature.tag())])?;

            match signature {
                type_system::Any::Primitive(_) => Ok(()),
            }
        },
    )?;

    out.flush()
}
