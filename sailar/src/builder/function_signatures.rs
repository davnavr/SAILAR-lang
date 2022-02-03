use crate::builder;
use crate::format;
use std::rc::Rc;

type Index = format::indices::FunctionSignature;

#[derive(Debug)]
#[non_exhaustive]
pub struct Function {
    index: Index,
    result_types: Vec<Rc<builder::Type>>,
    parameter_types: Vec<Rc<builder::Type>>,
}

impl Function {
    pub fn index(&self) -> Index {
        self.index
    }

    pub fn result_types(&self) -> &[Rc<builder::Type>] {
        &self.result_types
    }

    pub fn parameter_types(&self) -> &[Rc<builder::Type>] {
        &self.parameter_types
    }
}

pub struct Signatures {
    index: builder::counter::Cell<Index>,
    signatures: std::cell::RefCell<Vec<Rc<Function>>>,
}

impl Signatures {
    pub(super) fn new() -> Self {
        Self {
            index: builder::counter::Cell::new(),
            signatures: std::cell::RefCell::new(Vec::new()),
        }
    }

    pub fn insert(
        &self,
        result_types: Vec<Rc<builder::Type>>,
        parameter_types: Vec<Rc<builder::Type>>,
    ) -> Rc<Function> {
        let signature = Rc::new(Function {
            index: self.index.next(),
            result_types,
            parameter_types,
        });
        self.signatures.borrow_mut().push(signature.clone());
        signature
    }

    pub(super) fn build(&self) -> Vec<format::FunctionSignature> {
        fn collect_type_indices(
            types: &[Rc<builder::Type>],
        ) -> Vec<format::indices::TypeSignature> {
            types.iter().map(|signature| signature.index()).collect()
        }

        self.signatures
            .borrow_mut()
            .drain(..)
            .map(|signature| format::FunctionSignature {
                return_types: format::LenVec(collect_type_indices(&signature.result_types)),
                parameter_types: format::LenVec(collect_type_indices(&signature.parameter_types)),
            })
            .collect()
    }
}
