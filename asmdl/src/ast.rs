pub use registir::format::{
    instruction_set::{NumericType, OverflowBehavior},
    type_system::PrimitiveType,
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BitwiseOperation {
    pub result_type: Positioned<NumericType>,
    pub x: RegisterSymbol,
    pub y: RegisterSymbol,
}

/// Based on the registir instruction set, see `[registir::format::instruction_set::Instruction]` for more information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Nop,
    Ret(Vec<RegisterSymbol>),
    Phi(Vec<(Positioned<Vec<RegisterSymbol>>, LocalSymbol)>),
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
}
