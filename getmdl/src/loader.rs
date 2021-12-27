use registir::format;
use std::cell::{Ref, RefMut, RefCell};
use std::collections::{hash_map, HashMap};
use std::rc::{Rc, Weak};

pub use format::{Identifier, ModuleIdentifier};

struct ModuleData {
    source: format::Module,
    loaded_type_definitions: HashMap<format::indices::TypeDefinition, Rc<RefCell<Type>>>,
}

impl ModuleData {
    fn new(source: format::Module) -> Self {
        Self {
            source,
            loaded_type_definitions: HashMap::new(),
        }
    }
}

pub struct Module(Rc<RefCell<ModuleData>>);

impl Module {
    fn new(source: format::Module) -> Self {
        Self(Rc::new(RefCell::new(ModuleData::new(source))))
    }

    fn borrow(&self) -> Ref<'_, ModuleData> {
        self.0.borrow()
    }

    fn borrow_mut(&self) -> RefMut<'_, ModuleData> {
        self.0.borrow_mut()
    }
}

pub struct Type {
    module: Weak<RefCell<ModuleData>>,
    index: usize,
    specified_inherited_types: Vec<Rc<RefCell<Type>>>, // TODO: Cycle could be made here.
}

impl Type {
    fn new(module: Weak<RefCell<ModuleData>>, index: usize) -> Self {
        Self {
            module,
            specified_inherited_types: Vec::new(), // with_capacity
        }
    }
}

impl Module {
    pub fn load_type_definition_raw(
        &self,
        index: format::indices::TypeDefinition,
    ) -> Rc<RefCell<Type>> {
        match self.borrow_mut().loaded_type_definitions.entry(index) {
            hash_map::Entry::Vacant(vacant) => {
                let entry = Rc::new(RefCell::new(Type::new(Rc::downgrade(&self.0))));
                vacant.insert(entry.clone());
                entry
            }
            hash_map::Entry::Occupied(occupied) => occupied.get().clone(),
        }
    }
}

pub struct Loader {
    loaded_modules: HashMap<ModuleIdentifier, Rc<RefCell<Module>>>,
}

impl Loader {
    pub fn with_application(application: format::Module) -> (Self, Rc<RefCell<Module>>) {
        let mut loaded_modules = HashMap::new();
        let identifier = application.header.0.identifier.clone();
        let loaded_application = Rc::new(RefCell::new(Module::new(application)));
        loaded_modules.insert(identifier, loaded_application.clone());
        (Self { loaded_modules }, loaded_application)
    }
}
