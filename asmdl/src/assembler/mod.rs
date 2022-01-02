use crate::ast;
use registir::format;

mod declare;
mod indexed;

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum GlobalDeclaration {
    Code,
    TypeDefinition,
    MethodDefinition,
    MethodBody,
    EntryBlock,
    EntryPoint,
    Module,
    Format,
    Name,
    Namespace,
    /// A module or format version.
    Version,
}
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum LocalDeclaration {
    CodeBlock,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum NameError {
    Duplicate,
    Empty,
    Missing,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    DuplicateNamedDeclaration(ast::GlobalSymbol, GlobalDeclaration), // DuplicateGlobalDeclaration
    DuplicateLocalDeclaration(ast::LocalSymbol, LocalDeclaration),
    DuplicateRegisterDeclaration(ast::RegisterSymbol),
    DuplicateDeclaration(ast::Position, GlobalDeclaration),
    DuplicateModifier(ast::Position, &'static str),
    InvalidNameDeclaration(ast::Position, GlobalDeclaration, NameError),
    /// Used when a declaration that is required to be present, such as the module header, was not defined.
    MissingDeclaration(Option<ast::Position>, GlobalDeclaration),
    UndefinedGlobalSymbol(ast::GlobalSymbol, GlobalDeclaration),
    UndefinedLocalSymbol(ast::LocalSymbol, LocalDeclaration),
    UndefinedRegister(ast::RegisterSymbol),
    InvalidReturnRegisterCount {
        position: ast::Position,
        expected: u8,
        actual: usize,
    },
    InvalidConstantIntegerType(ast::Positioned<format::type_system::PrimitiveType>),
    PrimitiveConstantOutOfRange(format::type_system::PrimitiveType, ast::Positioned<i128>),
}

impl Error {
    pub fn position(&self) -> Option<&ast::Position> {
        match self {
            Self::DuplicateNamedDeclaration(
                ast::GlobalSymbol(ast::Positioned { position, .. }),
                _,
            )
            | Self::DuplicateLocalDeclaration(
                ast::LocalSymbol(ast::Positioned { position, .. }),
                _,
            )
            | Self::DuplicateRegisterDeclaration(ast::RegisterSymbol(ast::Positioned {
                position,
                ..
            }))
            | Self::InvalidReturnRegisterCount { position, .. }
            | Self::InvalidConstantIntegerType(ast::Positioned { position, .. })
            | Self::PrimitiveConstantOutOfRange(_, ast::Positioned { position, .. })
            | Self::UndefinedRegister(ast::RegisterSymbol(ast::Positioned { position, .. }))
            | Self::UndefinedGlobalSymbol(ast::GlobalSymbol(ast::Positioned { position, .. }), _)
            | Self::UndefinedLocalSymbol(ast::LocalSymbol(ast::Positioned { position, .. }), _)
            | Self::DuplicateDeclaration(position, _)
            | Self::DuplicateModifier(position, _)
            | Self::InvalidNameDeclaration(position, _, _) => Some(position),
            Self::MissingDeclaration(position, _) => position.as_ref(),
        }
    }
}

impl std::fmt::Display for GlobalDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Name => "name",
            Self::Namespace => "namespace",
            Self::TypeDefinition => "type definition",
            Self::MethodDefinition => "method definition",
            Self::MethodBody => "method body",
            Self::EntryBlock => "entry block",
            Self::EntryPoint => "entry point method",
            Self::Code => "code",
            Self::Module => "module",
            Self::Format => "format",
            Self::Version => "version",
        })
    }
}

impl std::fmt::Display for LocalDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::CodeBlock => "code block",
        })
    }
}

impl std::fmt::Display for NameError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Duplicate => "duplicate",
            Self::Missing => "missing",
            Self::Empty => "empty",
        })
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateNamedDeclaration(symbol, declaration) => write!(
                f,
                "a {} declaration corresponding to the symbol @{} already exists",
                declaration, symbol.0.value
            ),
            Self::DuplicateLocalDeclaration(symbol, declaration) => write!(
                f,
                "a {} corresponding to the symbol ${} already exists",
                declaration, symbol.0.value
            ),
            Self::DuplicateRegisterDeclaration(symbol) => write!(
                f,
                "a register corresponding to the symbol %{} already exists",
                symbol.0.value
            ),
            Self::DuplicateDeclaration(_, declaration) => {
                write!(f, "a {} declaration already exists", declaration)
            }
            Self::DuplicateModifier(_, name) => {
                write!(f, "the {} modifier was already specified", name)
            }
            Self::InvalidNameDeclaration(_, declaration, error) => {
                write!(f, "{} name for {} declaration", error, declaration)
            }
            Self::MissingDeclaration(_, declaration) => {
                write!(f, "missing {} declaration", declaration)
            }
            Self::UndefinedGlobalSymbol(symbol, declaration) => write!(
                f,
                "a {} corresponding to the symbol @{} could not be found",
                declaration, symbol.0.value
            ),
            Self::UndefinedLocalSymbol(symbol, declaration) => write!(
                f,
                "a {} corresponding to the symbol ${} could not be found",
                declaration, symbol.0.value
            ),
            Self::UndefinedRegister(symbol) => write!(
                f,
                "a register corresponding to the symbol %{} could not be found",
                symbol.0.value
            ),
            Self::InvalidReturnRegisterCount { expected, actual, .. } => write!(
                f,
                "expected at least {} temporary registers to contain the results of this instruction but got {}",
                expected,
                actual
            ),
            Self::InvalidConstantIntegerType(ast::Positioned { value: primitive_type, .. }) => write!(
                f,
                "{:?} is not a valid constant integer type",
                primitive_type
            ),
            Self::PrimitiveConstantOutOfRange(integer_type, ast::Positioned { value, .. }) => write!(
                f,
                "{} is not a valid {:?} constant",
                value,
                integer_type
            ),
        }
    }
}

fn assemble_module_header(
    errors: &mut Vec<Error>,
    declarations: &[ast::Positioned<ast::ModuleDeclaration>],
) -> Option<format::ModuleHeader> {
    let mut module_name = declare::Once::new(|name: &ast::Positioned<_>, _| {
        Error::InvalidNameDeclaration(
            name.position,
            GlobalDeclaration::Module,
            NameError::Duplicate,
        )
    });
    let mut module_version = declare::Once::new(|version: ast::Positioned<_>, _| {
        Error::DuplicateDeclaration(version.position, GlobalDeclaration::Version)
    });

    for node in declarations {
        match &node.value {
            ast::ModuleDeclaration::Name(name) => {
                if let Some(set_name) = module_name.declare(errors, name) {
                    set_name(match format::Identifier::try_from(&name.value.0) {
                        Ok(id) => Some(id),
                        Err(_) => {
                            errors.push(Error::InvalidNameDeclaration(
                                name.position,
                                GlobalDeclaration::Module,
                                NameError::Empty,
                            ));
                            None
                        }
                    })
                }
            }
            ast::ModuleDeclaration::Version(version) => {
                if let Some(set_version) = module_version.declare(
                    errors,
                    ast::Positioned {
                        value: version,
                        position: node.position,
                    },
                ) {
                    set_version(format::VersionNumbers(
                        format::structures::LengthEncodedVector(
                            version
                                .iter()
                                .map(|n| format::numeric::UInteger(*n))
                                .collect(),
                        ),
                    ))
                }
            }
        }
    }

    match module_name.value().flatten() {
        Some(name) => Some(format::ModuleHeader {
            identifier: format::ModuleIdentifier {
                name,
                version: module_version.value().unwrap_or_default(),
            },
        }),
        None => {
            errors.push(Error::MissingDeclaration(None, GlobalDeclaration::Name));
            None
        }
    }
}

fn assemble_module_format(
    errors: &mut Vec<Error>,
    declarations: &[ast::Positioned<ast::FormatDeclaration>],
) -> format::FormatVersion {
    fn declare_version<'a>() -> declare::Once<
        'a,
        ast::Position,
        format::numeric::UInteger,
        impl Fn(ast::Position, &format::numeric::UInteger) -> Error,
    > {
        declare::Once::new(|position, _| {
            Error::DuplicateDeclaration(position, GlobalDeclaration::Version)
        })
    }

    let mut major_version = declare_version();
    let mut minor_version = declare_version();

    for node in declarations {
        match &node.value {
            ast::FormatDeclaration::Major(major) => {
                major_version.declare_and_set(errors, node.position, *major)
            }
            ast::FormatDeclaration::Minor(minor) => {
                minor_version.declare_and_set(errors, node.position, *minor)
            }
        }
    }

    format::FormatVersion {
        major: major_version.value().unwrap_or_default(),
        minor: minor_version.value().unwrap_or_default(),
    }
}

type IdentifierLookup = indexed::Set<format::indices::Identifier, format::Identifier>;

type NamespaceLookup = indexed::Set<format::indices::Namespace, format::Namespace>;

type TypeSignatureLookup =
    indexed::Set<format::indices::TypeSignature, format::type_system::AnyType>;

type MethodSignatureLookup =
    indexed::Set<format::indices::MethodSignature, format::MethodSignature>;

type MethodBodyLookup<'a> =
    indexed::SymbolMap<'a, ast::GlobalSymbol, format::indices::Code, MethodBodyAssembler<'a>>;

type CodeBlockLookup<'a> = std::collections::HashMap<&'a ast::Identifier, usize>;

type MethodDefinitionLookup<'a> = indexed::SymbolMap<
    'a,
    ast::GlobalSymbol,
    format::indices::MethodDefinition,
    MethodDefinitionAssembler<'a>,
>;

fn visibility_declaration<'a>() -> declare::Once<
    'a,
    ast::Position,
    format::Visibility,
    impl Fn(ast::Position, &format::Visibility) -> Error,
> {
    declare::Once::new(|position, _| Error::DuplicateModifier(position, "visibility"))
}

type NameDeclaration<'a, 'b, E> =
    declare::Once<'a, &'b ast::Positioned<ast::LiteralString>, format::indices::Identifier, E>;

fn name_declaration<'a, 'b>() -> NameDeclaration<
    'a,
    'b,
    impl Fn(&'b ast::Positioned<ast::LiteralString>, &format::indices::Identifier) -> Error,
> {
    declare::Once::new(|node: &ast::Positioned<_>, _| {
        Error::InvalidNameDeclaration(
            node.position,
            GlobalDeclaration::TypeDefinition,
            NameError::Duplicate,
        )
    })
}

fn add_identifier_from(
    identifiers: &mut IdentifierLookup,
    declarer: GlobalDeclaration,
    name: &ast::Positioned<ast::LiteralString>,
) -> Result<format::indices::Identifier, Error> {
    format::Identifier::try_from(&name.value.0)
        .map(|id| identifiers.add(id))
        .map_err(|()| Error::InvalidNameDeclaration(name.position, declarer, NameError::Empty))
}

fn declare_name<
    'a,
    'b,
    E: Fn(&'b ast::Positioned<ast::LiteralString>, &format::indices::Identifier) -> Error,
>(
    declaration: &mut NameDeclaration<'a, 'b, E>,
    errors: &mut Vec<Error>,
    identifiers: &mut IdentifierLookup,
    declarer: GlobalDeclaration,
    name: &'b ast::Positioned<ast::LiteralString>,
) {
    if let Some(setter) = declaration.declare(errors, name) {
        match add_identifier_from(identifiers, declarer, name) {
            Ok(id) => setter(id),
            Err(error) => errors.push(error),
        }
    }
}

fn assemble_type_signature(
    #[allow(unused_mut, unused_variables)] errors: &mut Vec<Error>,
    signature: &ast::Positioned<ast::TypeSignature>,
) -> Option<format::TypeSignature> {
    match signature.value {
        ast::TypeSignature::Primitive(primitive_type) => {
            Some(format::TypeSignature::primitive(primitive_type))
        }
        ast::TypeSignature::Array(_) => todo!("Array types are not yet supported by asmdl"),
    }
}

fn collect_type_signatures(
    errors: &mut Vec<Error>,
    type_signatures: &mut TypeSignatureLookup,
    types: &[ast::Positioned<ast::TypeSignature>],
) -> Option<format::structures::LengthEncodedVector<format::indices::TypeSignature>> {
    let mut collected = Vec::with_capacity(types.len());
    let mut success = true;

    for signature in types {
        if let Some(assembled) = assemble_type_signature(errors, signature) {
            if success {
                collected.push(type_signatures.add(assembled));
            }
        } else {
            success = false;
        }
    }

    if success {
        Some(format::structures::LengthEncodedVector(collected))
    } else {
        None
    }
}

fn assemble_method_signature(
    errors: &mut Vec<Error>,
    type_signatures: &mut TypeSignatureLookup,
    parameter_types: &[ast::Positioned<ast::TypeSignature>],
    return_types: &[ast::Positioned<ast::TypeSignature>],
) -> Option<format::MethodSignature> {
    let parameters_signature = collect_type_signatures(errors, type_signatures, parameter_types);
    let return_signature = collect_type_signatures(errors, type_signatures, return_types);
    Some(format::MethodSignature {
        return_types: return_signature?,
        parameter_types: parameters_signature?,
    })
}

fn assemble_definitions<D, T, A: FnMut(&D) -> Option<T>>(
    definitions: &[D],
    mut assemble: A,
) -> Vec<T> {
    let mut assembled = Vec::with_capacity(definitions.len());
    let mut commit = true;
    for definition in definitions {
        match assemble(definition) {
            Some(result) => {
                if commit {
                    assembled.push(result)
                }
            }
            None => commit = false,
        }
    }
    assembled
}

#[derive(Debug)]
struct MethodBlockAssembler<'a> {
    name: &'a ast::Identifier,
    arguments: &'a [ast::RegisterSymbol],
    instructions: &'a [ast::Statement],
}

impl<'a> MethodBlockAssembler<'a> {
    fn lookup_register_index(
        errors: &mut Vec<Error>,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        register: &'a ast::RegisterSymbol,
    ) -> Option<format::indices::Register> {
        let index = register_lookup.get(register);
        if index.is_none() {
            errors.push(Error::UndefinedRegister(register.clone()))
        }
        index
    }

    fn lookup_register_indices(
        errors: &mut Vec<Error>,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        registers: &'a [ast::RegisterSymbol],
    ) -> Option<format::structures::LengthEncodedVector<format::indices::Register>> {
        let mut indices = Vec::with_capacity(registers.len());
        let mut success = true;

        for name in registers {
            if let Some(index) = Self::lookup_register_index(errors, register_lookup, name) {
                if success {
                    indices.push(index)
                }
            } else {
                success = false;
            }
        }

        if success {
            Some(format::structures::LengthEncodedVector(indices))
        } else {
            None
        }
    }

    fn convert_integer_constant<T, F: FnOnce(T) -> format::instruction_set::IntegerConstant>(
        value: &ast::Positioned<i128>,
        integer_type: format::instruction_set::PrimitiveType,
        constant: F,
    ) -> Result<format::instruction_set::IntegerConstant, Error>
    where
        T: TryFrom<i128, Error = std::num::TryFromIntError>,
    {
        T::try_from(value.value)
            .map(constant)
            .map_err(|_| Error::PrimitiveConstantOutOfRange(integer_type, value.clone()))
    }

    fn define_integer_constant(
        primitive_type: &ast::Positioned<format::instruction_set::PrimitiveType>,
        value: &ast::Positioned<i128>,
    ) -> Result<format::instruction_set::IntegerConstant, Error> {
        use format::instruction_set::{IntegerConstant, PrimitiveType};
        let integer_type = primitive_type.value;
        match integer_type {
            PrimitiveType::S32 => {
                Self::convert_integer_constant(value, integer_type, IntegerConstant::S32)
            }
            PrimitiveType::U32 => {
                Self::convert_integer_constant(value, integer_type, IntegerConstant::U32)
            }
            _ => Err(Error::InvalidConstantIntegerType(primitive_type.clone())),
        }
    }

    fn emit_basic_arithmetic_instruction<
        I: FnOnce(
            format::instruction_set::BasicArithmeticOperation,
        ) -> format::instruction_set::Instruction,
    >(
        errors: &mut Vec<Error>,
        operation: &'a ast::BasicArithmeticOperation,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        instructions: &mut Vec<format::instruction_set::Instruction>,
        instruction: I,
    ) {
        let registers = Self::lookup_register_index(errors, register_lookup, &operation.x).zip(
            Self::lookup_register_index(errors, register_lookup, &operation.y),
        );
        if let Some((x, y)) = registers {
            instructions.push(instruction(
                format::instruction_set::BasicArithmeticOperation {
                    overflow: ast::OverflowModifier::behavior(&operation.overflow_modifier),
                    return_type: operation.return_type.value,
                    x,
                    y,
                },
            ))
        }
    }

    fn define_division_instruction<
        I: FnOnce(format::instruction_set::DivisionOperation) -> format::instruction_set::Instruction,
    >(
        errors: &mut Vec<Error>,
        operation: &'a ast::DivisionOperation,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        instruction: I,
    ) -> Option<format::instruction_set::Instruction> {
        use format::instruction_set::DivideByZeroBehavior;

        let numerator = Self::lookup_register_index(errors, register_lookup, &operation.numerator)?;
        let denominator =
            Self::lookup_register_index(errors, register_lookup, &operation.denominator)?;

        let divide_by_zero_behavior = match operation.divide_by_zero_modifier {
            ast::DivideByZeroModifier::Halt => DivideByZeroBehavior::Halt,
            ast::DivideByZeroModifier::Return(ref nan) => DivideByZeroBehavior::Return(
                Self::lookup_register_index(errors, register_lookup, nan)?,
            ),
        };

        Some(instruction(format::instruction_set::DivisionOperation {
            divide_by_zero: divide_by_zero_behavior,
            overflow: ast::OverflowModifier::behavior(&operation.overflow_modifier),
            return_type: operation.return_type.value,
            numerator,
            denominator,
        }))
    }

    fn try_emit_instruction(
        definition: Option<format::instruction_set::Instruction>,
        instructions: &mut Vec<format::instruction_set::Instruction>,
    ) {
        if let Some(i) = definition {
            instructions.push(i)
        }
    }

    fn define_bitwise_instruction<
        I: FnOnce(format::instruction_set::BitwiseOperation) -> format::instruction_set::Instruction,
    >(
        errors: &mut Vec<Error>,
        operation: &'a ast::BitwiseOperation,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        instruction: I,
    ) -> Option<format::instruction_set::Instruction> {
        Some(instruction(format::instruction_set::BitwiseOperation {
            result_type: operation.result_type.value,
            x: Self::lookup_register_index(errors, register_lookup, &operation.x)?,
            y: Self::lookup_register_index(errors, register_lookup, &operation.y)?,
        }))
    }

    fn define_bitwise_shift_instruction<
        I: FnOnce(
            format::instruction_set::BitwiseShiftOperation,
        ) -> format::instruction_set::Instruction,
    >(
        errors: &mut Vec<Error>,
        operation: &'a ast::BitwiseOperation,
        register_lookup: &mut indexed::RegisterLookup<'a>,
        instruction: I,
    ) -> Option<format::instruction_set::Instruction> {
        Self::define_bitwise_instruction(errors, operation, register_lookup, |operation| {
            instruction(format::instruction_set::BitwiseShiftOperation(operation))
        })
    }

    fn lookup_jump_target(
        errors: &mut Vec<Error>,
        symbol: &'a ast::LocalSymbol,
        block_indices: &CodeBlockLookup<'a>,
    ) -> Option<format::instruction_set::JumpTarget> {
        if let Some(target) = block_indices.get(&symbol.0.value) {
            Some(format::indices::CodeBlock::try_from(*target).unwrap())
        } else {
            errors.push(Error::DuplicateLocalDeclaration(
                symbol.clone(),
                LocalDeclaration::CodeBlock,
            ));
            None
        }
    }

    fn assemble(
        &self,
        errors: &mut Vec<Error>,
        block_indices: &CodeBlockLookup<'a>,
        register_lookup: &mut indexed::RegisterLookup<'a>,
    ) -> Option<format::CodeBlock> {
        let error_count = errors.len();

        for register in self.arguments {
            if register_lookup.add_input(register).is_none() {
                errors.push(Error::DuplicateRegisterDeclaration(register.clone()))
            }
        }

        let mut instructions = Vec::with_capacity(self.instructions.len());

        for statement in self.instructions {
            use format::instruction_set::Instruction;

            let return_count = usize::from(statement.instruction.value.return_count());

            if statement.registers.len() <= return_count {
                match &statement.instruction.value {
                    ast::Instruction::Nop => instructions.push(Instruction::Nop),
                    ast::Instruction::Ret(registers) => {
                        if let Some(indices) =
                            Self::lookup_register_indices(errors, register_lookup, registers)
                        {
                            instructions.push(Instruction::Ret(indices));
                        }
                    }
                    ast::Instruction::Br(target, input_registers) => {
                        let mut define = || {
                            Some(Instruction::Br(
                                Self::lookup_jump_target(errors, target, block_indices)?,
                                Self::lookup_register_indices(
                                    errors,
                                    register_lookup,
                                    input_registers,
                                )?,
                            ))
                        };
                        Self::try_emit_instruction(define(), &mut instructions);
                    }
                    ast::Instruction::BrIf {
                        condition,
                        true_branch,
                        false_branch,
                        input_registers,
                    } => {
                        let mut define = || {
                            Some(Instruction::BrIf {
                                condition: Self::lookup_register_index(
                                    errors,
                                    register_lookup,
                                    condition,
                                )?,
                                true_branch: Self::lookup_jump_target(
                                    errors,
                                    true_branch,
                                    block_indices,
                                )?,
                                false_branch: Self::lookup_jump_target(
                                    errors,
                                    false_branch,
                                    block_indices,
                                )?,
                                input_registers: Self::lookup_register_indices(
                                    errors,
                                    register_lookup,
                                    input_registers,
                                )?,
                            })
                        };
                        Self::try_emit_instruction(define(), &mut instructions);
                    }
                    ast::Instruction::Add(operation) => Self::emit_basic_arithmetic_instruction(
                        errors,
                        operation,
                        register_lookup,
                        &mut instructions,
                        Instruction::Add,
                    ),
                    ast::Instruction::Sub(operation) => Self::emit_basic_arithmetic_instruction(
                        errors,
                        operation,
                        register_lookup,
                        &mut instructions,
                        Instruction::Sub,
                    ),
                    ast::Instruction::Mul(operation) => Self::emit_basic_arithmetic_instruction(
                        errors,
                        operation,
                        register_lookup,
                        &mut instructions,
                        Instruction::Mul,
                    ),
                    ast::Instruction::Div(operation) => Self::try_emit_instruction(
                        Self::define_division_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::Div,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::And(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::And,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::Or(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::Or,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::Not(result_type, value) => Self::try_emit_instruction(
                        register_lookup
                            .get(value)
                            .map(|register| Instruction::Not(result_type.value, register)),
                        &mut instructions,
                    ),
                    ast::Instruction::Xor(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::Xor,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::ShL(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_shift_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::ShL,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::ShR(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_shift_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::ShR,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::RotL(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_shift_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::RotL,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::RotR(operation) => Self::try_emit_instruction(
                        Self::define_bitwise_shift_instruction(
                            errors,
                            operation,
                            register_lookup,
                            Instruction::RotR,
                        ),
                        &mut instructions,
                    ),
                    ast::Instruction::ConstI(integer_type, value) => {
                        match Self::define_integer_constant(integer_type, value) {
                            Ok(constant) => instructions.push(Instruction::ConstI(constant)),
                            Err(error) => errors.push(error),
                        }
                    }
                }

                for name in &statement.registers {
                    if register_lookup.add_temporary(name).is_none() {
                        errors.push(Error::DuplicateRegisterDeclaration(name.clone()))
                    }
                }

                for _ in 0..(return_count - statement.registers.len()) {
                    register_lookup.ignore_temporary()
                }
            } else {
                errors.push(Error::InvalidReturnRegisterCount {
                    position: statement.instruction.position,
                    expected: statement.instruction.value.return_count().into(),
                    actual: statement.registers.len(),
                })
            }
        }

        if errors.len() <= error_count {
            Some(format::CodeBlock {
                input_register_count: format::numeric::UInteger(
                    u32::try_from(self.arguments.len()).unwrap(),
                ),
                exception_handler: None,
                instructions: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(instructions),
                ),
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct MethodBodyAssembler<'a> {
    origin: ast::Position,
    declarations: &'a [ast::Positioned<ast::CodeDeclaration>],
}

impl<'a> MethodBodyAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut Vec<Error>,
        #[allow(unused_variables)] defined_methods: &mut MethodDefinitionLookup,
        block_lookup: &mut CodeBlockLookup<'a>,
        blocks: &mut Vec<MethodBlockAssembler<'a>>,
        register_lookup: &mut indexed::RegisterLookup<'a>,
    ) -> Option<format::Code> {
        block_lookup.clear();
        blocks.clear();

        let mut entry_block = declare::Once::new(|position: ast::Position, _| {
            Error::DuplicateDeclaration(position, GlobalDeclaration::EntryBlock)
        });

        for declaration in self.declarations {
            match &declaration.value {
                ast::CodeDeclaration::Entry(symbol) => {
                    entry_block.declare_and_set(errors, declaration.position, symbol);
                }
                ast::CodeDeclaration::Block {
                    name,
                    arguments,
                    instructions,
                } => {
                    let block_name = &name.0.value;
                    if block_lookup.insert(block_name, blocks.len()).is_none() {
                        blocks.push(MethodBlockAssembler {
                            name: block_name,
                            arguments,
                            instructions,
                        })
                    } else {
                        errors.push(Error::DuplicateLocalDeclaration(
                            name.clone(),
                            LocalDeclaration::CodeBlock,
                        ));
                    }
                }
            }
        }

        match entry_block.value() {
            Some(entry_block_name) => {
                use std::collections::hash_map::Entry;

                match block_lookup.entry(&entry_block_name.0.value) {
                    Entry::Occupied(mut entry_block) => {
                        debug_assert!(!blocks.is_empty());
                        let entry_block_index = *entry_block.get();
                        let last_block_index = blocks.len() - 1;
                        let last_block_name = blocks[last_block_index].name;
                        blocks.swap(entry_block_index, last_block_index);

                        // Entry block is moved to last index, so lookup has to be adjusted.
                        let previous_entry_block_index = entry_block.insert(last_block_index);
                        *block_lookup.get_mut(last_block_name).unwrap() =
                            previous_entry_block_index;

                        let mut assembled_blocks = assemble_definitions(blocks, |block| {
                            // Reused when blocks are assembled.
                            register_lookup.clear();
                            block.assemble(errors, &block_lookup, register_lookup)
                        });

                        Some(format::Code {
                            entry_block: assembled_blocks.swap_remove(last_block_index),
                            blocks: format::structures::LengthEncodedVector(assembled_blocks),
                        })
                    }
                    Entry::Vacant(_) => {
                        errors.push(Error::UndefinedLocalSymbol(
                            entry_block_name.clone(),
                            LocalDeclaration::CodeBlock,
                        ));
                        None
                    }
                }
            }
            None => {
                errors.push(Error::MissingDeclaration(
                    Some(self.origin),
                    GlobalDeclaration::EntryBlock,
                ));
                None
            }
        }
    }
}

#[derive(Debug)]
struct MethodDefinitionAssembler<'a> {
    origin: ast::Position,
    owner: format::indices::TypeDefinition,
    parameter_types: &'a [ast::Positioned<ast::TypeSignature>],
    return_types: &'a [ast::Positioned<ast::TypeSignature>],
    modifiers: &'a [ast::Positioned<ast::MethodModifier>],
    declarations: &'a [ast::Positioned<ast::MethodDeclaration>],
}

impl<'a> MethodDefinitionAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut Vec<Error>,
        identifiers: &mut IdentifierLookup,
        type_signatures: &mut TypeSignatureLookup,
        method_signatures: &mut MethodSignatureLookup,
        method_bodies: &mut MethodBodyLookup,
    ) -> Option<format::Method> {
        let mut visibility = visibility_declaration();
        #[allow(unused_mut)]
        let mut flags = format::MethodFlags::default();

        for modifier in self.modifiers {
            match &modifier.value {
                ast::MethodModifier::Public => visibility.declare_and_set(
                    errors,
                    modifier.position,
                    format::Visibility::Public,
                ),
                ast::MethodModifier::Private => visibility.declare_and_set(
                    errors,
                    modifier.position,
                    format::Visibility::Private,
                ),
                ast::MethodModifier::Instance => todo!(),
                ast::MethodModifier::Initializer => todo!(),
            }
        }

        let mut method_name = name_declaration();
        let mut method_body = declare::Once::new(|position, _| {
            Error::DuplicateDeclaration(position, GlobalDeclaration::MethodBody)
        });

        for declaration in self.declarations {
            match &declaration.value {
                ast::MethodDeclaration::Name(name) => declare_name(
                    &mut method_name,
                    errors,
                    identifiers,
                    GlobalDeclaration::MethodDefinition,
                    name,
                ),
                ast::MethodDeclaration::Body(body) => {
                    if let Some(set_body) = method_body.declare(errors, declaration.position) {
                        set_body(match body {
                            ast::MethodBodyDeclaration::Defined(name) => {
                                match method_bodies.index_of(name) {
                                    Some(index) => Some(format::MethodBody::Defined(index)),
                                    None => {
                                        errors.push(Error::UndefinedGlobalSymbol(
                                            name.clone(),
                                            GlobalDeclaration::MethodBody,
                                        ));
                                        None
                                    }
                                }
                            }
                            ast::MethodBodyDeclaration::External { name, library } => {
                                unimplemented!()
                            }
                        })
                    }
                }
            }
        }

        if !method_name.is_set() {
            errors.push(Error::InvalidNameDeclaration(
                self.origin,
                GlobalDeclaration::MethodDefinition,
                NameError::Missing,
            ))
        }

        if !method_body.is_set() {
            errors.push(Error::MissingDeclaration(
                Some(self.origin),
                GlobalDeclaration::MethodBody,
            ))
        }

        if let (Some(name), Some(body)) = (method_name.value(), method_body.value().flatten()) {
            Some(format::Method {
                owner: self.owner,
                name,
                visibility: visibility.value().unwrap_or_default(),
                flags,
                signature: method_signatures.add(assemble_method_signature(
                    errors,
                    type_signatures,
                    self.parameter_types,
                    self.return_types,
                )?),
                body,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct TypeDefinitionAssembler<'a> {
    origin: ast::Position,
    index: format::indices::TypeDefinition,
    modifiers: &'a [ast::Positioned<ast::TypeModifier>],
    declarations: &'a [ast::Positioned<ast::TypeDeclaration>],
}

impl<'a> TypeDefinitionAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut Vec<Error>,
        identifiers: &mut IdentifierLookup,
        namespaces: &mut NamespaceLookup,
        methods: &mut MethodDefinitionLookup<'a>,
    ) -> Option<format::Type> {
        let mut visibility = visibility_declaration();
        #[allow(unused_mut)]
        let mut flags = format::TypeFlags::default();

        for modifier in self.modifiers {
            match &modifier.value {
                ast::TypeModifier::Public => visibility.declare_and_set(
                    errors,
                    modifier.position,
                    format::Visibility::Public,
                ),
                ast::TypeModifier::Private => visibility.declare_and_set(
                    errors,
                    modifier.position,
                    format::Visibility::Private,
                ),
            }
        }

        let mut type_name = name_declaration();
        let mut type_namespace = declare::Once::new(|position: ast::Position, _| {
            Error::DuplicateDeclaration(position, GlobalDeclaration::Namespace)
        });

        #[allow(unused_mut)]
        let mut field_indices = Vec::new();
        let mut method_indices = Vec::new();

        for declaration in self.declarations {
            match &declaration.value {
                ast::TypeDeclaration::Name(name) => declare_name(
                    &mut type_name,
                    errors,
                    identifiers,
                    GlobalDeclaration::TypeDefinition,
                    name,
                ),
                ast::TypeDeclaration::Namespace(namespace) => {
                    if let Some(set_namespace) =
                        type_namespace.declare(errors, declaration.position)
                    {
                        let mut indices = Vec::with_capacity(namespace.len());
                        let mut success = true;
                        for name in namespace {
                            match add_identifier_from(
                                identifiers,
                                GlobalDeclaration::TypeDefinition,
                                name,
                            ) {
                                Ok(id) => indices.push(id),
                                Err(error) => {
                                    errors.push(error);
                                    success = false;
                                }
                            }
                        }

                        if success {
                            set_namespace(
                                namespaces.add(format::structures::LengthEncodedVector(indices)),
                            )
                        }
                    }
                }
                ast::TypeDeclaration::Method {
                    symbol,
                    parameter_types,
                    return_types,
                    modifiers,
                    declarations,
                } => {
                    if let Some(index) = methods.try_add(
                        symbol,
                        MethodDefinitionAssembler {
                            origin: declaration.position,
                            owner: self.index,
                            modifiers,
                            declarations,
                            parameter_types,
                            return_types,
                        },
                    ) {
                        method_indices.push(index);
                    }
                }
            }
        }

        if !type_name.is_set() {
            errors.push(Error::InvalidNameDeclaration(
                self.origin,
                GlobalDeclaration::TypeDefinition,
                NameError::Missing,
            ))
        }

        if !type_namespace.is_set() {
            errors.push(Error::MissingDeclaration(
                Some(self.origin),
                GlobalDeclaration::Namespace,
            ))
        }

        if let (Some(name), Some(namespace)) = (type_name.value(), type_namespace.value()) {
            Some(format::Type {
                name,
                namespace,
                visibility: visibility.value().unwrap_or_default(),
                flags,
                layout: format::indices::TypeLayout(format::numeric::UInteger(0)), // TODO: Until `.layout` directives are supported, type layouts will be hard coded.
                inherited_types: format::structures::LengthEncodedVector(Vec::new()),
                fields: format::structures::LengthEncodedVector(field_indices),
                methods: format::structures::LengthEncodedVector(method_indices),
                vtable: format::structures::LengthEncodedVector(Vec::new()),
            })
        } else {
            None // Errors were already added
        }
    }
}

pub fn assemble_declarations(
    declarations: &[ast::Positioned<ast::TopLevelDeclaration>],
) -> Result<format::Module, Vec<Error>> {
    let mut errors = Vec::new();
    let mut module_header = None;
    let mut module_format = None;
    let mut module_entry_point = declare::Once::new(|(), symbol: &ast::GlobalSymbol| {
        Error::DuplicateDeclaration(symbol.0.position, GlobalDeclaration::EntryPoint)
    });
    let mut identifiers = IdentifierLookup::new();
    let mut namespaces = NamespaceLookup::new();
    let mut type_signatures = TypeSignatureLookup::new();
    let mut method_signatures = MethodSignatureLookup::new();
    let mut method_bodies = MethodBodyLookup::new();
    let mut type_definitions = indexed::SymbolMap::<
        ast::GlobalSymbol,
        format::indices::TypeDefinition,
        TypeDefinitionAssembler,
    >::new();
    let mut method_definitions = MethodDefinitionLookup::new();

    for node in declarations {
        match &node.value {
            ast::TopLevelDeclaration::Module(ref module_nodes) => {
                if module_header.is_none() {
                    module_header = Some(assemble_module_header(&mut errors, module_nodes))
                } else {
                    errors.push(Error::DuplicateDeclaration(
                        node.position,
                        GlobalDeclaration::Module,
                    ))
                }
            }
            ast::TopLevelDeclaration::Format(ref format_versions) => match module_format {
                None => module_format = Some(assemble_module_format(&mut errors, format_versions)),
                Some(_) => errors.push(Error::DuplicateDeclaration(
                    node.position,
                    GlobalDeclaration::Format,
                )),
            },
            ast::TopLevelDeclaration::Entry(ref entry_point_name) => {
                module_entry_point.declare_and_set(&mut errors, (), entry_point_name.clone())
            }
            ast::TopLevelDeclaration::Code {
                ref symbol,
                declarations,
            } => match method_bodies.try_add(
                symbol,
                MethodBodyAssembler {
                    origin: node.position,
                    declarations,
                },
            ) {
                Some(_) => (),
                None => errors.push(Error::DuplicateNamedDeclaration(
                    symbol.clone(),
                    GlobalDeclaration::Code,
                )),
            },
            ast::TopLevelDeclaration::Type {
                ref symbol,
                modifiers,
                declarations,
            } => match type_definitions.try_add_with(symbol, |index| TypeDefinitionAssembler {
                origin: node.position,
                index,
                modifiers,
                declarations,
            }) {
                Some(_) => (),
                None => errors.push(Error::DuplicateNamedDeclaration(
                    symbol.clone(),
                    GlobalDeclaration::TypeDefinition,
                )),
            },
            ref unknown => unimplemented!("{:?}", unknown),
        }
    }

    let assembled_method_bodies = {
        // These collections are reused as method bodies are assembled.
        let mut block_lookup = std::collections::HashMap::new();
        let mut register_lookup = indexed::RegisterLookup::new();
        let mut blocks = Vec::new();

        assemble_definitions(method_bodies.items(), |body| {
            body.assemble(
                &mut errors,
                &mut method_definitions,
                &mut block_lookup,
                &mut blocks,
                &mut register_lookup,
            )
        })
    };

    let assembled_type_definitions = assemble_definitions(type_definitions.items(), |definition| {
        definition.assemble(
            &mut errors,
            &mut identifiers,
            &mut namespaces,
            &mut method_definitions,
        )
    });

    let assembled_method_definitions =
        assemble_definitions(method_definitions.items(), |definition| {
            definition.assemble(
                &mut errors,
                &mut identifiers,
                &mut type_signatures,
                &mut method_signatures,
                &mut method_bodies,
            )
        });

    let entry_point_index = module_entry_point.value().and_then(|entry_point_name| {
        let index = method_definitions.index_of(&entry_point_name);
        if index.is_none() {
            errors.push(Error::UndefinedGlobalSymbol(
                entry_point_name,
                GlobalDeclaration::MethodDefinition,
            ));
        }
        index
    });

    if module_header.is_none() {
        errors.push(Error::MissingDeclaration(None, GlobalDeclaration::Module))
    }

    if errors.is_empty() {
        Ok(format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: module_format.unwrap_or_default(),
            // Some(None) would mean that the module header had no name, but an unwrap is safe here as an error should have been generated.
            header: format::structures::ByteLengthEncoded(module_header.flatten().unwrap()),
            // TODO: Add other things
            identifiers: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(identifiers.take_items()),
            ),
            namespaces: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(namespaces.take_items()),
            ),
            type_signatures: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(type_signatures.take_items()),
            ),
            method_signatures: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(method_signatures.take_items()),
            ),
            method_bodies: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(assembled_method_bodies),
            ),
            data_arrays: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(Vec::new()),
            ),
            imports: format::structures::ByteLengthEncoded(format::ModuleImports {
                imported_modules: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(Vec::new()),
                ),
                imported_types: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(Vec::new()),
                ),
                imported_fields: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(Vec::new()),
                ),
                imported_methods: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(Vec::new()),
                ),
            }),
            definitions: format::structures::ByteLengthEncoded(format::ModuleDefinitions {
                defined_types: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(assembled_type_definitions),
                ),
                defined_fields: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(Vec::new()),
                ),
                defined_methods: format::structures::ByteLengthEncoded(
                    format::structures::LengthEncodedVector(assembled_method_definitions),
                ),
            }),
            entry_point: format::structures::ByteLengthEncoded(entry_point_index),
            type_layouts: format::structures::ByteLengthEncoded(
                format::structures::LengthEncodedVector(vec![format::TypeLayout::Unspecified]),
            ),
        })
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use crate::{assembler, parser};
    use registir::format;

    #[test]
    fn module_header_test() {
        let declarations = parser::parse(".module { .name \"Test\"; .version 1 2 3; };").unwrap();
        let header = assembler::assemble_declarations(&declarations)
            .map(|module| module.header.0)
            .unwrap();

        assert_eq!(
            Ok(header.identifier.name),
            format::Identifier::try_from("Test")
        );
        assert_eq!(
            header.identifier.version,
            format::VersionNumbers::from_iter(vec![1u32, 2, 3].into_iter())
        );
    }
}
