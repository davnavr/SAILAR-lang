#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    pub fn chars(&self) -> &String {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct TryFromIdentifierError();

impl std::fmt::Display for TryFromIdentifierError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "identifiers cannot be empty")
    }
}

impl std::error::Error for TryFromIdentifierError {}

impl TryFrom<String> for Identifier {
    type Error = TryFromIdentifierError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err(TryFromIdentifierError())
        } else {
            Ok(Self(value))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub position: Position,
    pub name: Identifier,
}

#[derive(Debug)]
pub enum Declaration {
    Module { name: Identifier },
}
