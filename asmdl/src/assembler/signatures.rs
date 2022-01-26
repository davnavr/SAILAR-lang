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
        struct_lookup: &definitions::StructLookup<'a>,
    ) -> Result<format::indices::TypeSignature, Error> {
        self.lookup.insert_or_get_fallible(&ty.0, |_| match &ty.0 {
            ast::Type::Primitive(primitive_type) => {
                Ok(format::TypeSignature::Primitive(*primitive_type))
            }
            ast::Type::Struct(struct_name) => {
                let struct_identifier = struct_name.identifier();
                struct_lookup
                    .get_index(struct_identifier)
                    .ok_or_else(|| {
                        Error::with_location(
                            ErrorKind::UndefinedGlobal(struct_identifier.clone()),
                            ty.1.clone(),
                        )
                    })
                    .map(|index| {
                        format::TypeSignature::Struct(format::indices::Struct::Defined(index))
                    })
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
        errors: &mut error::Builder,
        type_lookup: &mut TypeLookup<'a>,
        struct_lookup: &definitions::StructLookup<'a>,
        parameter_types: ParameterSet<'a>,
        return_types: ParameterSet<'a>,
    ) -> Option<format::indices::FunctionSignature> {
        self.lookup
            .insert_or_get_fallible::<(), _>((parameter_types, return_types), |_| {
                let mut lookup_types = |types: ParameterSet<'a>| {
                    let mut indices = Vec::with_capacity(types.len());
                    let mut commit = true;

                    for ty in types {
                        match type_lookup.get(&ty, struct_lookup) {
                            Ok(index) if commit => indices.push(index),
                            Ok(_) => (),
                            Err(error) => {
                                commit = false;
                                errors.push(error);
                            }
                        }
                    }

                    if commit {
                        Ok(format::LenVec(indices))
                    } else {
                        Err(())
                    }
                };
                Ok(format::FunctionSignature {
                    parameter_types: lookup_types(parameter_types)?,
                    return_types: lookup_types(return_types)?,
                })
            })
            .ok()
    }
}
