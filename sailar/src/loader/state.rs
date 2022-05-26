//! Module for managing loader state.

use crate::loader::Module;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};

pub struct State {
    modules: Mutex<elsa::FrozenVec<Arc<Module>>>,
}

struct ModuleDebug<'s>(&'s Mutex<elsa::FrozenVec<Arc<Module>>>);

impl Debug for ModuleDebug<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0.try_lock() {
            Ok(fields) => f.debug_list().entries(fields.iter()).finish(),
            Err(error) => Debug::fmt(&error, f),
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State")
            .field("modules", &ModuleDebug(&self.modules))
            .finish()
    }
}
