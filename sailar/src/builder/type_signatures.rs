use crate::builder;
use crate::format::type_system;

pub struct Type<'b> {
    signature: type_system::Any,
    builder: builder::BuilderIdentifier<'b>,
}

pub struct Signatures<'b> {
    builder: builder::BuilderIdentifier<'b>,
    types: typed_arena::Arena<Type<'b>>,
    //primitive_lookup: hash_map::HashMap<type_system::Primitive, type_system::Any>,
}

impl<'b> Signatures<'b> {
    pub(super) fn new(builder: builder::BuilderIdentifier<'b>) -> Self {
        Self {
            builder,
            types: typed_arena::Arena::new(),
            //primitive_lookup: hash_map::HashMap::new(),
        }
    }

    pub fn insert_raw(&'b self, signature: type_system::Any) -> &'b Type<'b> {
        self.types.alloc(Type {
            signature,
            builder: self.builder,
        })
    }
}
