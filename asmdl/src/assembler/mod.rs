use crate::ast;
use registir::format;

mod error;
mod lookup;

pub use error::{Error, ErrorKind};

fn assemble_module_header(
    errors: &mut error::Builder,
    module_header: &mut Option<format::ModuleHeader>,
    declarations: &[ast::Positioned<ast::ModuleDeclaration>],
    location: &ast::Position,
) {
    let mut module_name = None;
    let mut module_version = None;

    for node in declarations {
        match &node.0 {
            ast::ModuleDeclaration::Name(name) => {
                if module_name.is_none() {
                    module_name = Some(name.0.clone())
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, node.1.clone())
                }
            }
            ast::ModuleDeclaration::Version(numbers) => {
                if module_version.is_none() {
                    module_version = Some(format::VersionNumbers::from(numbers.as_slice()));
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, node.1.clone())
                }
            }
        }
    }

    if module_header.is_some() {
        errors.push_with_location(ErrorKind::DuplicateDirective, location.clone())
    }

    if let Some(name) = module_name {
        *module_header = Some(format::ModuleHeader {
            identifier: format::ModuleIdentifier {
                name,
                version: module_version.unwrap_or_default(),
            },
        });
    } else {
        errors.push_with_location(
            ErrorKind::MissingDirective("module header name"),
            location.clone(),
        )
    }
}

fn assemble_module_format(
    errors: &mut error::Builder,
    module_format: &mut Option<format::FormatVersion>,
    declarations: &[ast::Positioned<ast::FormatDeclaration>],
    location: &ast::Position,
) {
    let mut major_version = None;
    let mut minor_version = None;

    for node in declarations {
        match &node.0 {
            ast::FormatDeclaration::Major(version) => {
                if major_version.is_none() {
                    major_version = Some(version)
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, node.1.clone())
                }
            }
            ast::FormatDeclaration::Minor(version) => {
                if minor_version.is_none() {
                    minor_version = Some(version)
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, node.1.clone())
                }
            }
        }
    }

    if major_version.is_none() {
        errors.push_with_location(
            ErrorKind::MissingDirective("major format version"),
            location.clone(),
        )
    }

    if minor_version.is_none() {
        errors.push_with_location(
            ErrorKind::MissingDirective("minor format version"),
            location.clone(),
        )
    }

    let format = format::FormatVersion {
        major: format::numeric::UInteger(*major_version.unwrap()),
        minor: format::numeric::UInteger(*minor_version.unwrap()),
    };

    if !format.is_supported() {
        errors.push_with_location(ErrorKind::InvalidFormatVersion, location.clone())
    }

    if module_format.is_none() {
        *module_format = Some(format);
    } else {
        errors.push_with_location(ErrorKind::DuplicateDirective, location.clone())
    }
}

fn assemble_items<D, T, A: FnMut(&D) -> Option<T>>(definitions: &[D], mut assemble: A) -> Vec<T> {
    let mut assembled = Vec::with_capacity(definitions.len());
    let mut commit = true;
    for definition in definitions {
        match assemble(definition) {
            Some(result) if commit => assembled.push(result),
            Some(_) => (),
            None => commit = false,
        }
    }
    assembled
}

struct CodeBlockAssembler<'a> {
    location: &'a ast::Position,
    input_registers: &'a [ast::RegisterSymbol],
    instructions: &'a [ast::Statement],
}

impl<'a> CodeBlockAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut error::Builder,
        _block_lookup: &CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> format::CodeBlock {
        fn lookup_register<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            register: &'a ast::RegisterSymbol,
        ) -> Option<format::indices::Register> {
            let index = register_lookup.get(register);
            if index.is_none() {
                errors.push_with_location(
                    ErrorKind::UndefinedRegister(register.identifier().clone()),
                    register.location().clone(),
                );
            }
            index
        }

        fn lookup_many_registers<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            registers: &'a [ast::RegisterSymbol],
        ) -> Option<format::LenVec<format::indices::Register>> {
            let mut indices = Vec::with_capacity(registers.len());
            let mut success = true;

            for name in registers {
                match lookup_register(errors, register_lookup, name) {
                    Some(index) if success => indices.push(index),
                    Some(_) => (),
                    None => success = false,
                }
            }

            if success {
                Some(format::LenVec(indices))
            } else {
                None
            }
        }

        register_lookup.clear();

        for register in self.input_registers {
            if let Err(original) = register_lookup.insert_input(register) {
                errors.push_with_location(
                    ErrorKind::DuplicateRegister {
                        name: register.identifier().clone(),
                        original,
                    },
                    register.location().clone(),
                );
            }
        }

        let mut instructions = Vec::with_capacity(self.instructions.len());

        for statement in self.instructions {
            use format::instruction_set::Instruction;

            let expected_return_count;
            let next_instruction;
            match &statement.instruction.0 {
                ast::Instruction::Nop => {
                    expected_return_count = 0;
                    next_instruction = Some(Instruction::Nop);
                }
                ast::Instruction::Ret(registers) => {
                    expected_return_count = 0;
                    next_instruction = lookup_many_registers(errors, register_lookup, registers)
                        .map(Instruction::Ret);
                }
                ast::Instruction::ConstI(constant_type, value) => {
                    use format::instruction_set::{IntegerConstant, PrimitiveType};

                    macro_rules! convert_constant {
                        ($destination_type: ty, $constant: expr) => {
                            <$destination_type>::try_from(value.0)
                                .map($constant)
                                .map_err(|_| {
                                    Error::new(
                                        ErrorKind::ConstantIntegerOutOfRange(
                                            constant_type.0,
                                            value.0,
                                        ),
                                        Some(value.1.clone()),
                                    )
                                })
                        };
                    }

                    let constant = match constant_type.0 {
                        PrimitiveType::S8 => convert_constant!(i8, IntegerConstant::S8),
                        PrimitiveType::U8 => convert_constant!(u8, IntegerConstant::U8),
                        PrimitiveType::S16 => convert_constant!(i16, IntegerConstant::S16),
                        PrimitiveType::U16 => convert_constant!(u16, IntegerConstant::U16),
                        PrimitiveType::S32 => convert_constant!(i32, IntegerConstant::S32),
                        PrimitiveType::U32 => convert_constant!(u32, IntegerConstant::U32),
                        PrimitiveType::S64 => convert_constant!(i64, IntegerConstant::S64),
                        PrimitiveType::U64 => convert_constant!(u64, IntegerConstant::U64),
                        invalid_type => Err(Error::new(
                            ErrorKind::InvalidConstantIntegerType(invalid_type),
                            Some(constant_type.1.clone()),
                        )),
                    };

                    next_instruction = match constant {
                        Ok(literal) => Some(Instruction::ConstI(literal)),
                        Err(error) => {
                            errors.push(error);
                            None
                        }
                    };
                    expected_return_count = 1;
                }
            }

            let return_registers = &statement.results.0;
            let actual_return_count = return_registers.len();

            if actual_return_count == expected_return_count {
                if let Some(instruction) = next_instruction {
                    instructions.push(instruction)
                }
            } else {
                errors.push_with_location(
                    ErrorKind::InvalidReturnRegisterCount {
                        expected: expected_return_count,
                        actual: actual_return_count,
                    },
                    statement.results.1.clone(),
                )
            }

            for name in &statement.results.0 {
                if let Err(error) = register_lookup.insert_temporary(name) {
                    errors.push_with_location(
                        ErrorKind::DuplicateRegister {
                            name: name.identifier().clone(),
                            original: error,
                        },
                        name.location().clone(),
                    )
                }
            }
        }

        format::CodeBlock {
            input_register_count: format::numeric::UInteger(
                self.input_registers.len().try_into().unwrap(),
            ),
            exception_handler: None,
            instructions: format::LenVecBytes::from(instructions),
        }
    }
}

struct FunctionBodyAssembler<'a> {
    location: &'a ast::Position,
    declarations: &'a [ast::Positioned<ast::CodeDeclaration>],
}

impl<'a> FunctionBodyAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut error::Builder,
        block_lookup: &mut CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> Option<format::Code> {
        block_lookup.clear();
        let mut entry_block_symbol = None;

        for declaration in self.declarations {
            match &declaration.0 {
                ast::CodeDeclaration::Entry(symbol) => {
                    if entry_block_symbol.is_none() {
                        entry_block_symbol = Some(symbol)
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
                ast::CodeDeclaration::Block {
                    name,
                    arguments: input_registers,
                    instructions,
                } => {
                    let block_name = name.identifier();
                    if let Err(existing) = block_lookup.insert(
                        block_name,
                        (
                            block_name,
                            CodeBlockAssembler {
                                input_registers,
                                instructions,
                                location: &declaration.1,
                            },
                        ),
                    ) {
                        errors.push_with_location(
                            ErrorKind::DuplicateBlock {
                                name: block_name.clone(),
                                original: existing.1.location.clone(),
                            },
                            declaration.1.clone(),
                        )
                    }
                }
            }
        }

        if let Some(entry_block_name) = entry_block_symbol {
            Some(format::Code {
                entry_block: block_lookup
                    .swap_remove(entry_block_name.identifier())
                    .expect("entry block should exist in lookup")
                    .assemble(errors, block_lookup, register_lookup),
                blocks: format::LenVec(assemble_items(block_lookup.values(), |(_, block)| {
                    Some(block.assemble(errors, block_lookup, register_lookup))
                })),
            })
        } else {
            errors.push_with_location(
                ErrorKind::MissingDirective("missing entry block"),
                self.location.clone(),
            );
            None
        }
    }
}

struct FunctionDefinitionAssembler<'a> {
    location: &'a ast::Position,
    export_symbol: &'a Option<ast::Positioned<ast::Identifier>>,
    declarations: &'a [ast::Positioned<ast::FunctionDeclaration>],
}

impl<'a> FunctionDefinitionAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut error::Builder,
        symbols: &mut SymbolLookup<'a>,
        identifiers: &mut IdentifierLookup,
        code_lookup: &mut FunctionCodeLookup<'a>,
    ) -> Option<format::Function> {
        let export_symbol_index = self.export_symbol.as_ref().map(|(symbol, location)| {
            if let Some(existing) = symbols.insert(symbol, location) {
                errors.push_with_location(
                    ErrorKind::DuplicateSymbol {
                        symbol: symbol.clone(),
                        original: existing.clone(),
                    },
                    location.clone(),
                )
            }

            identifiers.insert_or_get(symbol.clone())
        });

        let mut function_name = None;
        let mut function_body = None;

        for declaration in self.declarations {
            match &declaration.0 {
                ast::FunctionDeclaration::Name((name, _)) => if function_name.is_none() {
                    function_name = Some(identifiers.insert_or_get(name.clone()))
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, declaration.1.clone())
                },
            }
        }

        if function_name.is_none() {
            errors.push_with_location(ErrorKind::MissingDirective("function name"), self.location.clone())
        }

        if function_body.is_none() {
            errors.push_with_location(ErrorKind::MissingDirective("function body"), self.location.clone())
        }

        if let (Some(name), Some(body)) = (function_name, function_body) {
            Some(format::Function {
                name,
                symbol: export_symbol_index,
                signature: todo!(),
                body
            })
        }
        else {
            None
        }
    }
}

type IdentifierLookup = lookup::IndexedSet<format::indices::Identifier, format::Identifier>;
//type NamespaceLookup
//type TypeSignatureLookup
//type FunctionSignatureLookup

type CodeBlockLookup<'a> =
    lookup::IndexedMap<'a, u32, (&'a ast::Identifier, CodeBlockAssembler<'a>)>;

type FunctionCodeLookup<'a> =
    lookup::IndexedMap<'a, format::indices::Code, FunctionBodyAssembler<'a>>;

type SymbolLookup<'a> = std::collections::HashMap<&'a ast::Identifier, &'a ast::Position>;

type FunctionDefinitionLookup<'a> =
    lookup::IndexedMap<'a, format::indices::FunctionDefinition, FunctionDefinitionAssembler<'a>>;

pub fn assemble_declarations<'a>(
    declarations: &'a [ast::Positioned<ast::TopLevelDeclaration>],
) -> Result<format::Module, Vec<Error>> {
    let mut errors = error::Builder::new();
    let mut module_header = None;
    let mut module_format = None;
    let mut identifiers = IdentifierLookup::new();
    //let mut namespaces
    //let mut type_signatures
    //let mut function_signatures
    let mut function_bodies = FunctionCodeLookup::new();

    let mut function_definitions = FunctionDefinitionLookup::new();

    let mut entry_point_name = None;

    for node in declarations {
        match &node.0 {
            ast::TopLevelDeclaration::Module(declarations) => {
                assemble_module_header(&mut errors, &mut module_header, &declarations, &node.1)
            }
            ast::TopLevelDeclaration::Format(declarations) => {
                assemble_module_format(&mut errors, &mut module_format, &declarations, &node.1)
            }
            ast::TopLevelDeclaration::Code {
                symbol,
                declarations,
            } => {
                if let Err(error) = function_bodies.insert(
                    symbol.identifier(),
                    FunctionBodyAssembler {
                        declarations,
                        location: &node.1,
                    },
                ) {
                    errors.push_with_location(
                        ErrorKind::DuplicateDeclaration {
                            name: symbol.identifier().clone(),
                            kind: "code block",
                            original: error.location.clone(),
                        },
                        node.1.clone(),
                    );
                }
            }
            ast::TopLevelDeclaration::Function {
                symbol,
                exported: export_symbol,
                declarations,
            } => {
                if let Err(error) = function_definitions.insert(
                    symbol.identifier(),
                    FunctionDefinitionAssembler {
                        declarations,
                        location: &node.1,
                        export_symbol,
                    },
                ) {
                    errors.push_with_location(
                        ErrorKind::DuplicateDeclaration {
                            name: symbol.identifier().clone(),
                            kind: "function definition",
                            original: error.location.clone(),
                        },
                        node.1.clone(),
                    );
                }
            }
            ast::TopLevelDeclaration::Entry(name) => {
                if entry_point_name.is_none() {
                    entry_point_name = Some(name);
                } else {
                    errors.push_with_location(ErrorKind::DuplicateDirective, node.1.clone())
                }
            }
        }
    }

    if module_header.is_none() {
        errors.push(Error::from(ErrorKind::MissingDirective(
            "module header is required",
        )));
    }

    if module_format.is_none() {
        errors.push(Error::from(ErrorKind::MissingDirective(
            "module format is required",
        )));
    }

    let module;
    if let (Some(header), Some(format)) = (module_header, module_format) {
        let assembled_function_bodies = {
            // These collections are reused as function bodies are assembled.
            let mut block_lookup = CodeBlockLookup::new();
            let mut register_map = lookup::RegisterMap::new();

            assemble_items(&function_bodies.values(), |body| {
                body.assemble(&mut errors, &mut block_lookup, &mut register_map)
            })
        };

        // NOTE: Should ALL symbols in a module be unique, or will a type with the same symbol as a function with the same symbol as a global be allowed?
        // Currently, all symbols are unique.
        let mut symbol_lookup = SymbolLookup::new();

        let assembled_function_definitions =
            assemble_items(&function_definitions.values(), |definition| {
                definition.assemble(&mut errors, &mut symbol_lookup, &mut identifiers, &mut function_bodies)
            });

        module = Some(format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: format,
            header: format::LenBytes(header),
            identifiers: format::LenVecBytes::from(identifiers.into_vec()),
            namespaces: format::LenVecBytes::from(Vec::new()),
            type_signatures: format::LenVecBytes::from(Vec::new()),
            function_signatures: format::LenVecBytes::from(Vec::new()),
            function_bodies: format::LenVecBytes::from(assembled_function_bodies),
            data: format::LenVecBytes::from(Vec::new()),
            imports: format::LenBytes(format::ModuleImports {
                imported_modules: format::LenVecBytes::from(Vec::new()),
                imported_structs: format::LenVecBytes::from(Vec::new()),
                imported_globals: format::LenVecBytes::from(Vec::new()),
                imported_fields: format::LenVecBytes::from(Vec::new()),
                imported_functions: format::LenVecBytes::from(Vec::new()),
            }),
            definitions: format::LenBytes(format::ModuleDefinitions {
                defined_structs: format::LenVecBytes::from(Vec::new()),
                defined_globals: format::LenVecBytes::from(Vec::new()),
                defined_fields: format::LenVecBytes::from(Vec::new()),
                defined_functions: format::LenVecBytes::from(assembled_function_definitions),
            }),
            struct_layouts: format::LenVecBytes::from(Vec::new()),
            entry_point: format::LenBytes(None), // TODO: Look up entry point
        });
    } else {
        module = None;
    }

    match module {
        Some(result) if errors.is_empty() => Ok(result),
        _ => {
            assert!(!errors.is_empty());
            Err(errors.into_vec())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{assembler, parser};
    use registir::format;

    macro_rules! assert_success {
        ($input: expr, $checker: expr) => {{
            let (tree, lexer_errors, parser_errors) = parser::tree_from_str($input);
            assert_eq!((lexer_errors, parser_errors), (Vec::new(), Vec::new()));
            $checker(assembler::assemble_declarations(&tree).unwrap());
        }};
    }

    #[test]
    fn module_header_test() {
        assert_success!(
            ".module { .name \"Test\"; .version 1 2 3; };",
            |module: format::Module| {
                let header = module.header.0;
                assert_eq!(
                    Ok(header.identifier.name),
                    format::Identifier::try_from("Test")
                );
                assert_eq!(
                    header.identifier.version,
                    format::VersionNumbers::from_iter(vec![1u32, 2, 3].into_iter())
                );
            }
        )
    }
}
