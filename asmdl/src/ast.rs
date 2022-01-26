pub use registir::format::{
    instruction_set::{ComparisonKind, OverflowBehavior},
    type_system::Primitive as PrimitiveType,
    Identifier,
};

pub type Position = std::ops::Range<usize>;
pub type Positioned<T> = (T, Position);

macro_rules! symbol_type {
    ($symbol_type: ident) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $symbol_type(pub Positioned<Identifier>);

        impl $symbol_type {
            pub fn identifier(&self) -> &Identifier {
                &self.0 .0
            }

            pub fn location(&self) -> &Position {
                &self.0 .1
            }
        }

        impl From<$symbol_type> for Identifier {
            fn from(identifier: $symbol_type) -> Identifier {
                identifier.0 .0
            }
        }
    };
}

symbol_type!(RegisterSymbol);
symbol_type!(LocalSymbol);
symbol_type!(GlobalSymbol);

#[derive(Clone, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LiteralString(pub Vec<char>);

impl std::fmt::Display for LiteralString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.0 {
            match *c {
                '\t' => f.write_str(r"\t")?,
                '\n' => f.write_str(r"\n")?,
                '\r' => f.write_str(r"\r")?,
                '\\' => f.write_str(r"\\")?,
                '\"' => f.write_str("\\\"")?,
                c if c.is_control() => write!(f, "\\u{:04X}", u32::from(c))?,
                _ => write!(f, "{}", c)?,
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for LiteralString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self)
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
pub enum Type {
    Primitive(PrimitiveType),
    Struct(GlobalSymbol),
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Primitive(primitive_type) => {
                state.write_u8(0);
                primitive_type.hash(state)
            }
            Self::Struct(name) => {
                state.write_u8(0xDE);
                name.identifier().hash(state)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicArithmeticOperation {
    pub x: RegisterSymbol,
    pub y: RegisterSymbol,
    pub flag_overflow: bool,
}

impl BasicArithmeticOperation {
    pub fn return_count(&self) -> usize {
        if self.flag_overflow {
            2
        } else {
            1
        }
    }
}

/// Based on the registir instruction set, see `[registir::format::instruction_set::Instruction]` for more information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Nop,
    Ret(Vec<RegisterSymbol>),
    //Select
    Switch {
        comparison: RegisterSymbol,
        comparison_type: Positioned<PrimitiveType>,
        default_target: LocalSymbol,
        targets: Vec<(Positioned<i128>, LocalSymbol)>,
    },
    Br {
        target: LocalSymbol,
        inputs: Vec<RegisterSymbol>,
    },
    BrIf {
        condition: RegisterSymbol,
        true_branch: LocalSymbol,
        false_branch: LocalSymbol,
        inputs: Vec<RegisterSymbol>,
    },
    Call {
        function: GlobalSymbol,
        arguments: Vec<RegisterSymbol>,
    },
    Add(BasicArithmeticOperation),
    Sub(BasicArithmeticOperation),
    Mul(BasicArithmeticOperation),
    ConstI(Positioned<PrimitiveType>, Positioned<i128>),
    ConvI {
        operand: RegisterSymbol,
        target_type: Positioned<PrimitiveType>,
        flag_overflow: bool,
    },
    Cmp {
        x: RegisterSymbol,
        kind: ComparisonKind,
        y: RegisterSymbol,
    },
    Field {
        field: GlobalSymbol,
        declaring_struct: GlobalSymbol,
        object: RegisterSymbol,
    },
    Alloca {
        amount: RegisterSymbol,
        element_type: Positioned<Type>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub results: Positioned<Vec<RegisterSymbol>>,
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
pub enum FunctionBodyDeclaration {
    Defined(GlobalSymbol),
    External {
        library: Positioned<Identifier>,
        name: Positioned<Identifier>,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FunctionDeclaration {
    Name(Positioned<Identifier>),
    Body(FunctionBodyDeclaration),
}

#[derive(Debug, Eq, PartialEq)]
pub enum FieldDeclaration {
    Name(Positioned<Identifier>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum StructLayoutDeclaration {
    Unspecified,
}

#[derive(Debug, Eq, PartialEq)]
pub enum StructDeclaration {
    Name(Positioned<Identifier>),
    Field {
        symbol: GlobalSymbol,
        value_type: Positioned<Type>,
        is_export: bool,
        declarations: Vec<Positioned<FieldDeclaration>>,
    },
    Layout(StructLayoutDeclaration),
}

#[derive(Debug, Eq, PartialEq)]
pub enum FormatDeclaration {
    Major(u32),
    Minor(u32),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ModuleDeclaration {
    Name(Positioned<Identifier>),
    Version(Vec<u32>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopLevelDeclaration {
    Format(Vec<Positioned<FormatDeclaration>>),
    Module(Vec<Positioned<ModuleDeclaration>>),
    Entry(GlobalSymbol),
    Code {
        symbol: GlobalSymbol,
        declarations: Vec<Positioned<CodeDeclaration>>,
    },
    Function {
        symbol: GlobalSymbol,
        is_export: bool,
        parameter_types: Vec<Positioned<Type>>,
        return_types: Vec<Positioned<Type>>,
        declarations: Vec<Positioned<FunctionDeclaration>>,
    },
    Struct {
        symbol: GlobalSymbol,
        is_export: bool,
        declarations: Vec<Positioned<StructDeclaration>>,
    },
}
