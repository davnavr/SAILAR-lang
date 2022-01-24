use super::*;

/// Represents a type signature.
pub struct Type<'a> {
    source: &'a format::TypeSignature,
    size: Cell<Option<usize>>,
    module: &'a Module<'a>,
}

impl<'a> Type<'a> {
    pub(super) fn new(module: &'a Module<'a>, source: &'a format::TypeSignature) -> Self {
        Self {
            module,
            source,
            size: Cell::new(None),
        }
    }

    pub fn declaring_module(&self) -> &'a Module<'a> {
        self.module
    }

    pub fn as_raw(&self) -> &'a format::TypeSignature {
        self.source
    }

    fn calculate_size(&self) -> usize {
        use format::type_system::PrimitiveType;

        match self.source {
            format::TypeSignature::Primitive(primitive) => match primitive {
                PrimitiveType::U8 | PrimitiveType::S8 => 1,
                PrimitiveType::U16 | PrimitiveType::S16 => 2,
                PrimitiveType::U32 | PrimitiveType::S32 | PrimitiveType::F32 => 4,
                PrimitiveType::U64 | PrimitiveType::S64 | PrimitiveType::F64 => 8,
                PrimitiveType::UNative | PrimitiveType::SNative => std::mem::size_of::<isize>(),
            },
            _ => todo!("size calculation not implemented"),
        }
    }

    pub fn size(&self) -> usize {
        match self.size.get() {
            Some(size) => size,
            None => {
                let size = self.calculate_size();
                self.size.set(Some(size));
                size
            }
        }
    }
}
