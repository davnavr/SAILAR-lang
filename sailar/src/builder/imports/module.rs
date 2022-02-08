use crate::builder;
use crate::format;
use std::cell::RefCell;
use std::collections::hash_map;
use std::rc::Rc;

pub struct Imports {
    imports: RefCell<Vec<Rc<Module>>>,
    lookup: RefCell<hash_map::HashMap<format::ModuleIdentifier, Rc<Module>>>,
    index: builder::counter::Cell<format::indices::Module>,
}

impl Imports {
    pub(super) fn new() -> Self {
        Self {
            imports: RefCell::new(Vec::new()),
            lookup: RefCell::new(hash_map::HashMap::new()),
            index: builder::counter::Cell::with_start_value(1),
        }
    }

    pub fn get_or_import(&self, identifier: &format::ModuleIdentifier) -> Rc<Module> {
        match self.lookup.borrow_mut().entry(identifier.clone()) {
            hash_map::Entry::Occupied(occupied) => occupied.get().clone(),
            hash_map::Entry::Vacant(vacant) => {
                let module = Rc::new(Module {
                    index: self.index.next(),
                    identifier: identifier.clone(),
                    hash: RefCell::default(),
                });

                self.imports.borrow_mut().push(module.clone());
                vacant.insert(module).clone()
            }
        }
    }

    pub(super) fn build(&self) -> Vec<format::ModuleImport> {
        self.imports
            .borrow_mut()
            .drain(..)
            .map(|import| format::ModuleImport {
                hash: import.hash.take(),
                identifier: match Rc::try_unwrap(import) {
                    Ok(owned) => owned.identifier,
                    Err(import) => import.identifier.clone(),
                },
            })
            .collect()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Module {
    index: format::indices::Module,
    identifier: format::ModuleIdentifier,
    hash: RefCell<format::ModuleHash>,
}

impl Module {
    pub fn index(&self) -> format::indices::Module {
        self.index
    }

    //Might be nicer to have function, field, global import helpers be here, since modules logically own them.
    //pub fn import_function

    pub fn set_hash(&self, hash: format::ModuleHash) {
        self.hash.replace(hash);
    }

    pub fn identifier(&self) -> &format::ModuleIdentifier {
        &self.identifier
    }
}
