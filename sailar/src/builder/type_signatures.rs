use crate::builder;
use crate::format::{self, type_system};
use std::cell::RefCell;
use std::collections::hash_map;
use std::rc::Rc;

type Index = format::indices::TypeSignature;

type IndexCounter = Rc<builder::counter::Cell<Index>>;

pub struct Type {
    index: std::cell::Cell<Option<Index>>,
    next_available_index: IndexCounter,
    signature: type_system::Any,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.signature, f)
    }
}

impl std::cmp::PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        self.signature == other.signature
    }
}

impl Type {
    pub fn index(&self) -> Index {
        match self.index.get() {
            Some(existing) => existing,
            None => {
                let index = self.next_available_index.next();
                self.index.set(Some(index));
                index
            }
        }
    }
}

pub struct Signatures {
    index: IndexCounter,
    types: RefCell<Vec<Rc<Type>>>,
    primitive_lookup: RefCell<hash_map::HashMap<type_system::Primitive, Rc<Type>>>,
}

impl Signatures {
    pub fn new() -> Self {
        Self {
            index: IndexCounter::new(builder::counter::Cell::new()),
            types: RefCell::new(Vec::new()),
            primitive_lookup: RefCell::new(hash_map::HashMap::new()),
        }
    }

    pub fn insert_raw(&self, signature: type_system::Any) -> Rc<Type> {
        let type_signature = Rc::new(Type {
            index: std::cell::Cell::new(None),
            next_available_index: self.index.clone(),
            signature,
        });
        self.types.borrow_mut().push(type_signature.clone());
        type_signature
    }

    pub fn primitive_type<T: Into<type_system::Primitive>>(&self, primitive: T) -> Rc<Type> {
        let primitive_type = primitive.into();
        match self.primitive_lookup.borrow_mut().entry(primitive_type) {
            hash_map::Entry::Occupied(existing) => existing.get().clone(),
            hash_map::Entry::Vacant(vacant) => vacant
                .insert(self.insert_raw(type_system::Any::Primitive(primitive_type)))
                .clone(),
        }
    }

    pub(super) fn build(&self) -> Vec<format::TypeSignature> {
        // Assigning an index lazily means that types should be sorted to ensure the types match up with their indices.
        let mut types = std::cell::RefCell::take(&self.types);

        // Probably a faster way to remove types without an index while also sorting at the same time.
        types.sort_unstable_by_key(|type_signature| type_signature.index.get());

        types
            .into_iter()
            .filter_map(|type_signature| {
                if type_signature.index.get().is_some() {
                    Some(type_signature.signature.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}
