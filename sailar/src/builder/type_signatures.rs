use crate::format::type_system;

// #[derive(Clone)]
// pub struct Type {
//     signature: type_system::Any,
//     //module: usize,
// }

pub type Type = type_system::Any;

pub struct Signatures {
    types: typed_arena::Arena<type_system::Any>,
    //primitive_lookup: hash_map::HashMap<type_system::Primitive, type_system::Any>,
}

impl Signatures {
    pub(super) fn new(/* module: usize */) -> Self {
        Self {
            types: typed_arena::Arena::new(),
            //primitive_lookup: hash_map::HashMap::new(),
        }
    }

    pub fn insert_raw(&mut self, signature: type_system::Any) -> &Type {
        self.types.alloc(signature)
    }
}
