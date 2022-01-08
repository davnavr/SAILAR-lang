pub use registir::format::{
    instruction_set::{NumericType, OverflowBehavior, TailCall},
    type_system::PrimitiveType,
    Identifier,
};

pub type Position = std::ops::Range<usize>;
pub type Positioned<T> = (T, Position);

macro_rules! symbol_type {
    ($symbol_type: ident) => {
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct $symbol_type(pub Positioned<Identifier>);

        impl From<$symbol_type> for Identifier {
            fn from(identifier: $symbol_type) -> Identifier {
                identifier.0 .0
            }
        }

        impl<'a> From<&'a $symbol_type> for &'a Identifier {
            fn from(identifier: &'a $symbol_type) -> Self {
                &identifier.0 .0
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
pub enum TypeSignature {
    Primitive(PrimitiveType),
    Array(Box<TypeSignature>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OverflowModifier {
    Halt,
    Flag,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicArithmeticOperation {
    pub return_type: Positioned<NumericType>,
    pub x: RegisterSymbol,
    pub y: RegisterSymbol,
    pub overflow_modifier: Option<Positioned<OverflowModifier>>,
}

impl OverflowModifier {
    pub fn behavior(modifier: &Option<Positioned<Self>>) -> OverflowBehavior {
        match modifier {
            Some((OverflowModifier::Halt, _)) => OverflowBehavior::Halt,
            Some((OverflowModifier::Flag, _)) => OverflowBehavior::Flag,
            None => OverflowBehavior::Ignore,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DivideByZeroModifier {
    Return(RegisterSymbol),
    Halt,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DivisionOperation {
    pub return_type: Positioned<NumericType>,
    pub numerator: RegisterSymbol,
    pub denominator: RegisterSymbol,
    pub overflow_modifier: Option<Positioned<OverflowModifier>>,
    pub divide_by_zero_modifier: DivideByZeroModifier,
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
    ConstI(Positioned<PrimitiveType>, Positioned<i128>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub results: Vec<RegisterSymbol>,
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
    Major(i128),
    Minor(i128),
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
        exported: Option<Positioned<Identifier>>,
        declarations: Vec<Positioned<FunctionDeclaration>>,
    },
}
