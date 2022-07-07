//! Contains types representing errors encountered during loading.

use crate::module::Module;
use sailar::index;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

macro_rules! not_found_struct {
    ($(#[$meta:meta])* $vis:vis struct $name:ident($index:ty);) => {
        $(#[$meta])*
        $vis struct $name {
            index: $index,
            module: Arc<Module>,
        }

        impl $name {
            pub(crate) fn new(index: $index, module: Arc<Module>) -> Self {
                Self { index, module }
            }

            pub fn index(&self) -> $index {
                self.index
            }

            /// Gets the module that was used to resolve the index.
            pub fn module(&self) -> &Arc<Module> {
                &self.module
            }
        }
    };
}

not_found_struct! {
    #[derive(Clone, Debug, thiserror::Error)]
    pub struct TypeSignatureNotFoundError(index::TypeSignature);
}

impl Display for TypeSignatureNotFoundError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "a type signature corresponding to the index {:?} could not be found in module",
            self.index
        )
    }
}
