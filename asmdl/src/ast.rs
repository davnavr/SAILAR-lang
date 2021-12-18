#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Positioned<T> {
    pub position: Position,
    pub value: T,
}

impl<T> Positioned<T> {
    pub fn new(line: u32, column: u32, value: T) -> Positioned<T> {
        Self {
            position: Position { line, column },
            value,
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct LiteralString(pub Vec<char>);

impl std::fmt::Display for LiteralString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.0 {
            let c = *c;
            if c.is_control() || c == '\t' || c == '\\' || c == '\"' {
                write!(f, "\\u{:04X}", u32::from(c))?;
            } else {
                write!(f, "{}", c)?;
            }
        }
        Ok(())
    }
}

impl From<&str> for LiteralString {
    fn from(s: &str) -> Self {
        Self(s.chars().collect())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum FormatDeclaration {
    Major(u8),
    Minor(u8),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ModuleDeclaration {
    Name(LiteralString),
    Version(Vec<u64>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Format(Vec<Positioned<FormatDeclaration>>),
    Module(Vec<Positioned<ModuleDeclaration>>),
}
