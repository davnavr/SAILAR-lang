use sailar::format;

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
    Other(#[from] Box<dyn std::error::Error>),
}
