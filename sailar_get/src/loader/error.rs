use sailar::format;

#[derive(thiserror::Error, Debug)]
#[error("the imported function {symbol} imported in {import_signature:?} at index {import} and defined in {defining_module:?} at index {definition} was expected to have the signature {import_signature:?} from the importing module's point of view, but the actual signature was {definition_signature:?} from the imported module's point of view")]
#[non_exhaustive]
pub struct FunctionImportSignatureMismatch {
    pub symbol: format::Identifier,
    pub import: format::indices::FunctionImport,
    pub import_signature: format::FunctionSignature,
    pub importing_module: format::ModuleIdentifier,
    pub definition: format::indices::FunctionDefinition,
    pub definition_signature: format::FunctionSignature,
    pub defining_module: format::ModuleIdentifier,
}

#[derive(thiserror::Error, Debug)]
#[error("expected {expected:?} but actual input types were {actual:?}")]
#[non_exhaustive]
pub struct InputTypeMismatchError {
    pub expected: Vec<format::TypeSignature>,
    pub actual: Vec<format::TypeSignature>,
}

// TODO: Box all large error cases, split them into separate structs.
#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("index {0} is out of bounds")]
    IndexOutOfBounds(format::numeric::UInteger),
    #[error(
        "expected field {field} to be owned by struct {expected} but actual owner was {actual}"
    )]
    FieldOwnerMismatch {
        field: format::indices::FieldDefinition,
        expected: format::indices::StructDefinition,
        actual: format::indices::StructDefinition,
    },
    #[error("duplicated field {field} in struct {owner}")]
    DuplicateField {
        field: format::indices::FieldDefinition,
        owner: format::indices::StructDefinition,
    },
    #[error("the module {0:?} could not be loaded")]
    ModuleNotFound(format::ModuleIdentifier),
    #[error("the import at index {index} corresponding to the symbol {symbol} could not be found")]
    ImportNotFound {
        index: format::numeric::UInteger,
        symbol: format::Identifier,
    },
    #[error(transparent)]
    FunctionImportSignatureMismatch(#[from] Box<FunctionImportSignatureMismatch>),
    #[error(transparent)]
    InputTypeMismatch(#[from] Box<InputTypeMismatchError>),
    #[error("pointers of size {0} bytes are not supported")]
    InvalidPointerSize(crate::loader::PointerSize),
    #[error(transparent)]
    Other(#[from] Box<dyn std::error::Error>),
}

macro_rules! boxed_error_conversion {
    ($source_type: ty, $case_name: ident) => {
        impl From<$source_type> for Error {
            fn from(mismatch: $source_type) -> Self {
                Self::$case_name(Box::new(mismatch))
            }
        }
    };
}

boxed_error_conversion!(
    FunctionImportSignatureMismatch,
    FunctionImportSignatureMismatch
);

boxed_error_conversion!(InputTypeMismatchError, InputTypeMismatch);
