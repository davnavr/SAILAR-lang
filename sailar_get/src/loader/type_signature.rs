use crate::loader::{self, Result};
use sailar::format::{self, type_system};

/// Represents a type signature.
pub struct Type<'a> {
    source: &'a format::TypeSignature,
    index: format::indices::TypeSignature,
    size: std::cell::Cell<Option<u32>>,
    module: &'a loader::Module<'a>,
}

impl<'a> Type<'a> {
    pub(super) fn new(
        module: &'a loader::Module<'a>,
        source: &'a format::TypeSignature,
        index: format::indices::TypeSignature,
    ) -> Self {
        Self {
            module,
            source,
            index,
            size: std::cell::Cell::new(None),
        }
    }

    pub fn index(&'a self) -> format::indices::TypeSignature {
        self.index
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.module
    }

    pub fn as_raw(&'a self) -> &'a format::TypeSignature {
        self.source
    }

    fn calculate_size(&'a self) -> Result<u32> {
        use format::type_system::{FixedInt, Int, Primitive, Real};

        Ok(match self.source {
            format::TypeSignature::Primitive(primitive) => match primitive {
                Primitive::Int(Int::Fixed(FixedInt::U8 | FixedInt::S8)) => 1,
                Primitive::Int(Int::Fixed(FixedInt::U16 | FixedInt::S16)) => 2,
                Primitive::Int(Int::Fixed(FixedInt::U32 | FixedInt::S32))
                | Primitive::Real(Real::F32) => 4,
                Primitive::Int(Int::Fixed(FixedInt::U64 | FixedInt::S64))
                | Primitive::Real(Real::F64) => 8,
                Primitive::Int(Int::UNative | Int::SNative) => {
                    u32::from(self.module.loader().pointer_size().get())
                }
            },
            format::TypeSignature::NativePointer(_) => {
                u32::from(self.module.loader().pointer_size().get())
            }
            format::TypeSignature::Struct(struct_index) => {
                self.module.load_struct_raw(*struct_index)?.total_size()?
            }
        })
    }

    pub fn size(&'a self) -> Result<u32> {
        Ok(match self.size.get() {
            Some(size) => size,
            None => {
                let size = self.calculate_size()?;
                self.size.set(Some(size));
                size
            }
        })
    }
}

fn compare_raw_types<'a>(
    x_module: &'a loader::Module<'a>,
    x: &'a type_system::Any,
    y_module: &'a loader::Module<'a>,
    y: &'a type_system::Any,
) -> bool {
    match (x, y) {
        (type_system::Any::Primitive(x), type_system::Any::Primitive(y)) => x == y,
        (type_system::Any::Struct(x), type_system::Any::Struct(y)) => todo!(
            "loading of structs is not yet implemented, so checking of struct types is not too"
        ),
        (type_system::Any::NativePointer(x), type_system::Any::NativePointer(y)) => {
            compare_raw_types(x_module, x, y_module, y)
        }
        _ => false,
    }
}

impl<'a> std::cmp::PartialEq for &'a Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        // Check if both types are defiend in the same module
        if std::ptr::eq(self.module, other.module) {
            // Loaded modules ensure that loaded type signatures are unique, so comparison by signature is allowed.
            self.index == other.index
        } else {
            compare_raw_types(
                self.declaring_module(),
                self.source,
                other.declaring_module(),
                other.source,
            )
        }
    }
}

impl<'a> std::cmp::Eq for &'a Type<'a> {}
