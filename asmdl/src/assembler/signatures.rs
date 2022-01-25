use crate::assembler::*;

pub struct TypeLookup<'a> {
    lookup:
        lookup::IndexedMap<format::indices::TypeSignature, &'a ast::Type, format::TypeSignature>,
}

impl<'a> TypeLookup<'a> {
    pub fn new() -> Self {
        Self {
            lookup: lookup::IndexedMap::new(),
        }
    }

    pub fn drain_to_vec(&mut self) -> Vec<format::TypeSignature> {
        self.lookup.drain_to_vec()
    }

    pub(crate) fn get(
        &mut self,
        ty: &'a ast::Positioned<ast::Type>,
    ) -> format::indices::TypeSignature {
        self.lookup.insert_or_get_with(&ty.0, |_| match &ty.0 {
            ast::Type::Primitive(primitive_type) => {
                format::TypeSignature::Primitive(*primitive_type)
            }
        })
    }
}

pub type ParameterSet<'a> = &'a [ast::Positioned<ast::Type>];

pub struct FunctionLookup<'a> {
    lookup: lookup::IndexedMap<
        format::indices::FunctionSignature,
        (ParameterSet<'a>, ParameterSet<'a>),
        format::FunctionSignature,
    >,
}

impl<'a> FunctionLookup<'a> {
    pub fn new() -> Self {
        Self {
            lookup: lookup::IndexedMap::new(),
        }
    }

    pub fn drain_to_vec(&mut self) -> Vec<format::FunctionSignature> {
        self.lookup.drain_to_vec()
    }

    pub(crate) fn get(
        &mut self,
        type_lookup: &mut TypeLookup<'a>,
        parameter_types: ParameterSet<'a>,
        return_types: ParameterSet<'a>,
    ) -> format::indices::FunctionSignature {
        self.lookup
            .insert_or_get_with((parameter_types, return_types), |_| {
                let mut lookup_types = |types: ParameterSet<'a>| {
                    let mut indices = Vec::with_capacity(types.len());
                    indices.extend(types.iter().map(|ty| type_lookup.get(ty)));
                    format::LenVec(indices)
                };
                format::FunctionSignature {
                    parameter_types: lookup_types(parameter_types),
                    return_types: lookup_types(return_types),
                }
            })
    }
}
