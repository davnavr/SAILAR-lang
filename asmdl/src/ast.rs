pub use registir::format::type_system::PrimitiveType;

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

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    pub fn chars(&self) -> &String {
        &self.0
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
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
    pub value: T,
    pub position: Position,
}

impl<T> Positioned<T> {
    pub fn new(line: u32, column: u32, value: T) -> Positioned<T> {
        Self {
            position: Position { line, column },
            value,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, mapper: F) -> Positioned<U> {
        Positioned {
            position: self.position,
            value: mapper(self.value),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RegisterSymbol(pub Positioned<Identifier>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LocalSymbol(pub Positioned<Identifier>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GlobalSymbol(pub Positioned<Identifier>);

macro_rules! symbol_from_identifier {
    ($symbol_type: ty) => {
        impl From<$symbol_type> for Identifier {
            fn from(identifier: $symbol_type) -> Identifier {
                identifier.0.value
            }
        }

        impl<'a> From<&'a $symbol_type> for &'a Identifier {
            fn from(id: &'a $symbol_type) -> Self {
                &id.0.value
            }
        }
    };
}

symbol_from_identifier!(RegisterSymbol);
symbol_from_identifier!(LocalSymbol);
symbol_from_identifier!(GlobalSymbol);

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

impl From<LiteralString> for String {
    fn from(s: LiteralString) -> Self {
        s.0.iter().collect()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeSignature {
    Primitive(PrimitiveType),
    Array(Box<TypeSignature>),
}

/// Based on the registir instruction set, see `[registir::format::instruction_set::Instruction]` for more information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Nop,
    ConstZero(PrimitiveType),
    Ret(Vec<RegisterSymbol>),
}

impl Instruction {
    /// Returns the number of registers that should be on the left side of the `=` sign.
    pub fn return_count(&self) -> u8 {
        match self {
            Self::Nop | Self::Ret(_) => 0,
            Self::ConstZero(_) => 1,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub registers: Vec<RegisterSymbol>,
    pub instruction: Positioned<Instruction>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum CodeDeclaration {
    /// Specifies the block that is the entry block.
    Entry(LocalSymbol),
    Block {
        name: LocalSymbol,
        arguments: Vec<RegisterSymbol>,
        instructions: Vec<Statement>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum DataKind {
    //Bytes(Vec<Positioned<ByteDataDeclaration>>),
    String {
        content: LiteralString,
        //encoding: StringDataEncoding,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MethodModifier {
    Public,
    Private,
    Instance,
    Initializer,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TypeModifier {
    Public,
    Private,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MethodBodyDeclaration {
    Defined(GlobalSymbol),
    External {
        library: Positioned<LiteralString>,
        name: Positioned<LiteralString>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MethodDeclaration {
    Name(Positioned<LiteralString>),
    Body(MethodBodyDeclaration),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeDeclaration {
    Name(Positioned<LiteralString>),
    Namespace(Vec<Positioned<LiteralString>>),
    Method {
        symbol: GlobalSymbol,
        parameter_types: Vec<Positioned<TypeSignature>>,
        return_types: Vec<Positioned<TypeSignature>>,
        modifiers: Vec<Positioned<MethodModifier>>,
        declarations: Vec<Positioned<MethodDeclaration>>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FormatDeclaration {
    Major(registir::format::numeric::UInteger),
    Minor(registir::format::numeric::UInteger),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ModuleDeclaration {
    Name(Positioned<LiteralString>),
    Version(Vec<u32>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Format(Vec<Positioned<FormatDeclaration>>),
    Module(Vec<Positioned<ModuleDeclaration>>),
    Code {
        symbol: GlobalSymbol,
        declarations: Vec<Positioned<CodeDeclaration>>,
    },
    Data {
        symbol: GlobalSymbol,
        kind: DataKind,
    },
    Type {
        symbol: GlobalSymbol,
        modifiers: Vec<Positioned<TypeModifier>>,
        declarations: Vec<Positioned<TypeDeclaration>>,
    },
}
