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

    pub fn size(&self) -> usize {
        fn calculate<'a>(signature: &'a format::TypeSignature) -> usize {
            use format::type_system::{FixedInt, Int, Primitive, Real};

            match signature {
                format::TypeSignature::Primitive(primitive) => match primitive {
                    Primitive::Int(Int::Fixed(FixedInt::U8 | FixedInt::S8)) => 1,
                    Primitive::Int(Int::Fixed(FixedInt::U16 | FixedInt::S16)) => 2,
                    Primitive::Int(Int::Fixed(FixedInt::U32 | FixedInt::S32))
                    | Primitive::Real(Real::F32) => 4,
                    Primitive::Int(Int::Fixed(FixedInt::U64 | FixedInt::S64))
                    | Primitive::Real(Real::F64) => 8,
                    Primitive::Int(Int::UNative | Int::SNative) => std::mem::size_of::<isize>(),
                },
                format::TypeSignature::NativePointer(_) => std::mem::size_of::<*mut u8>(),
                format::TypeSignature::Struct(_) => {
                    todo!("size calculation not implemented for structs")
                }
            }
        }

        match self.size.get() {
            Some(size) => size,
            None => {
                let size = calculate(self.source);
                self.size.set(Some(size));
                size
            }
        }
    }
}
