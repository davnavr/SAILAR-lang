use crate::loader::{self, Result};
use sailar::format;

/// Represents a type signature.
pub struct Type<'a> {
    source: &'a format::TypeSignature,
    size: std::cell::Cell<Option<u32>>,
    module: &'a loader::Module<'a>,
}

impl<'a> Type<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::TypeSignature) -> Self {
        Self {
            module,
            source,
            size: std::cell::Cell::new(None),
        }
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
                Primitive::Int(Int::UNative | Int::SNative) => u32::from(self.module.loader().pointer_size()),
            },
            format::TypeSignature::NativePointer(_) => u32::from(self.module.loader().pointer_size()),
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
