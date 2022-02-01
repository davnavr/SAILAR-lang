use crate::builder;
use crate::format::{self, type_system};
use std::collections::hash_map;

pub struct Signatures {
    //Vec<Rc<Type>> would ensure that unused types are removed
    index: builder::counter::Cell<format::indices::TypeSignature>,
    types: typed_arena::Arena<type_system::Any>,
    primitive_lookup: hash_map::HashMap<type_system::Primitive, type_system::Any>,
}

impl Signatures {
    pub fn new() -> Self {
        Self {
            index: builder::counter::Cell::new(),
            types: typed_arena::Arena::new(),
            primitive_lookup: hash_map::HashMap::new(),
        }
    }

    pub fn build(&mut self) -> Vec<format::TypeSignature> {
        std::mem::take(&mut self.types).into_vec()
    }
}

impl<'a> builder::TypeSignatures<'a> {
    pub fn insert_raw(&'a mut self, signature: type_system::Any) -> builder::Type<'a> {
        let type_signature = self.signatures.types.alloc(signature);
        builder::Type {
            index: self.signatures.index.next(),
            signature: type_signature,
        }
    }

    pub fn primitive<'b, T: Into<type_system::Primitive>>(
        &'b self,
        primitive: T,
    ) -> builder::Type<'a>
    where
        'a: 'b,
    {
        // // TODO: How to ensure indices are not out of order?
        // let signature = self.signatures.primitive_lookup.entry(primitive.into()).or_insert_with_key(|primitive_type| {
        //     type_system::Any::Primitive(*primitive_type)
        // });

        // builder::Type {
        //     index: self.signatures.index.next(),
        //     signature,
        // }
        builder::Type {
            index: self.signatures.index.next(),
            signature: self
                .signatures
                .types
                .alloc(type_system::Any::Primitive(primitive.into())),
        }
    }
}
