use crate::ast;
use sailar::format;

mod error;
mod lookup;

pub use error::{Error, Kind as ErrorKind};

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

mod code_gen;
mod definitions;
mod signatures;

type IdentifierLookup = lookup::IndexedSet<format::indices::Identifier, format::Identifier>;

//type NamespaceLookup

type SymbolLookup<'a> = std::collections::HashMap<&'a ast::Identifier, &'a ast::Position>;

pub fn assemble_declarations(
    declarations: &[ast::Positioned<ast::TopLevelDeclaration>],
) -> Result<format::Module, Vec<Error>> {
    let mut errors = error::Builder::new();
    let mut module_header = None;
    let mut module_format = None;
    let mut identifiers = IdentifierLookup::new();
    //let mut namespaces
    let mut type_signatures = signatures::TypeLookup::new();
    let mut function_signatures = signatures::FunctionLookup::new();
    let mut function_bodies = code_gen::FunctionCodeLookup::new();

    let mut function_definitions = definitions::FunctionLookup::new();
    let mut field_definitions = definitions::FieldLookup::new();
    let mut struct_definitions = definitions::StructLookup::new();
    let mut struct_layouts = definitions::StructLayoutLookup::new();

    let mut entry_point_name = None;

    for node in declarations {
        match &node.0 {
            ast::TopLevelDeclaration::Module(declarations) => {
                assemble_module_header(&mut errors, &mut module_header, declarations, &node.1)
            }
            ast::TopLevelDeclaration::Format(declarations) => {
                assemble_module_format(&mut errors, &mut module_format, declarations, &node.1)
            }
            ast::TopLevelDeclaration::Code {
                symbol,
                declarations,
            } => {
                if let Err(error) = function_bodies.insert(
                    symbol.identifier(),
                    code_gen::FunctionBodyAssembler {
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
                is_export,
                parameter_types,
                return_types,
                declarations,
            } => {
                let symbol_name = symbol.identifier();
                if let Err(error) = function_definitions.insert(
                    symbol_name,
                    definitions::FunctionAssembler {
                        declarations,
                        symbol: symbol_name,
                        is_export: *is_export,
                        location: &node.1,
                        parameter_types,
                        return_types,
                    },
                ) {
                    errors.push_with_location(
                        ErrorKind::DuplicateDeclaration {
                            name: symbol_name.clone(),
                            kind: "function definition",
                            original: error.location.clone(),
                        },
                        node.1.clone(),
                    );
                }
            }
            ast::TopLevelDeclaration::Struct {
                symbol,
                is_export,
                declarations,
            } => {
                let symbol_name = symbol.identifier();
                if let Err(error) = struct_definitions.insert_with(symbol_name, |index| {
                    definitions::StructAssembler {
                        index,
                        declarations,
                        is_export: *is_export,
                        location: &node.1,
                        symbol: symbol_name,
                    }
                }) {
                    errors.push_with_location(
                        ErrorKind::DuplicateDeclaration {
                            name: symbol_name.clone(),
                            kind: "struct definition",
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
        // NOTE: Should ALL symbols in a module be unique, or will a type with the same symbol as a function with the same symbol as a global be allowed?
        // Currently, all symbols are unique.
        let mut symbol_lookup = SymbolLookup::new();

        let assembled_struct_definitions =
            assemble_items(struct_definitions.values(), |definition| {
                definition.assemble(
                    &mut errors,
                    &mut symbol_lookup,
                    &mut identifiers,
                    &mut struct_layouts,
                    &mut field_definitions,
                )
            });

        let assembled_field_definitions =
            assemble_items(field_definitions.values(), |definition| {
                definition.assemble(
                    &mut errors,
                    &mut symbol_lookup,
                    &mut identifiers,
                    &mut type_signatures,
                    &struct_definitions,
                )
            });

        let assembled_function_definitions =
            assemble_items(function_definitions.values(), |definition| {
                definition.assemble(
                    &mut errors,
                    &mut symbol_lookup,
                    &mut identifiers,
                    &mut function_bodies,
                    &mut type_signatures,
                    &struct_definitions,
                    &mut function_signatures,
                )
            });

        // Must come after definitions are assembled, to allow their use in function bodies.
        let assembled_function_bodies = {
            // These collections are reused as function bodies are assembled.
            let mut block_lookup = code_gen::CodeBlockLookup::new();
            let mut register_map = lookup::RegisterMap::new();

            assemble_items(function_bodies.values(), |body| {
                body.assemble(
                    &mut errors,
                    &mut type_signatures,
                    &struct_definitions,
                    &field_definitions,
                    &mut function_definitions,
                    &mut block_lookup,
                    &mut register_map,
                )
            })
        };

        let entry_point_index = entry_point_name.and_then(|name| {
            let result = function_definitions.get_index(name.identifier());
            if result.is_none() {
                errors.push_with_location(
                    ErrorKind::UndefinedGlobal(name.identifier().clone()),
                    name.location().clone(),
                );
            }
            result
        });

        module = Some(format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: format,
            header: format::LenBytes(header),
            identifiers: format::LenVecBytes::from(identifiers.drain_to_vec()),
            namespaces: format::LenVecBytes::from(Vec::new()),
            type_signatures: format::LenVecBytes::from(type_signatures.drain_to_vec()),
            function_signatures: format::LenVecBytes::from(function_signatures.drain_to_vec()),
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
                defined_structs: format::LenVecBytes::from(assembled_struct_definitions),
                defined_globals: format::LenVecBytes::from(Vec::new()),
                defined_fields: format::LenVecBytes::from(assembled_field_definitions),
                defined_functions: format::LenVecBytes::from(assembled_function_definitions),
            }),
            struct_layouts: format::LenVecBytes::from(struct_layouts.drain_to_vec()),
            entry_point: format::LenBytes(entry_point_index),
        });
    } else {
        module = None;
    }

    match module {
        Some(result) if errors.is_empty() => Ok(result),
        _ => {
            assert!(!errors.is_empty());
            Err(errors.drain_to_vec())
        }
    }
}

pub fn assemble_from_str(
    input: &str,
) -> Result<
    format::Module,
    (
        Vec<crate::lexer::Error>,
        Vec<crate::parser::Error>,
        Vec<Error>,
    ),
> {
    let (tree, lexer_errors, parser_errors) = crate::parser::tree_from_str(input);
    let module;
    let assembler_errors;

    match assemble_declarations(&tree) {
        Ok(assembled) => {
            module = Some(assembled);
            assembler_errors = Vec::new();
        }
        Err(errors) => {
            module = None;
            assembler_errors = errors;
        }
    }

    match module {
        Some(assembled) if lexer_errors.is_empty() && parser_errors.is_empty() => Ok(assembled),
        _ => Err((lexer_errors, parser_errors, assembler_errors)),
    }
}

#[cfg(test)]
mod tests {
    use crate::assembler;
    use sailar::format::{
        self,
        indices::{Register, TemporaryRegister},
        instruction_set::{BlockIndex, Instruction, IntegerConstant},
    };

    macro_rules! assert_success {
        ($input: expr, $checker: expr) => {
            $checker(assembler::assemble_from_str($input).unwrap())
        };
    }

    #[test]
    fn module_header_test() {
        assert_success!(
            ".module { .name \"Test\"; .version 1 2 3; };\n.format { .major 0; .minor 8; };",
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
                assert!(module.format_version.is_supported());
            }
        )
    }

    #[test]
    fn return_sample_compiles_successfully() {
        assert_success!(
            include_str!(r"../../../sailas/samples/return.txtmdl"),
            |module: format::Module| {
                let code = &module.function_bodies[0];
                assert_eq!(
                    code.entry_block.instructions.0 .0,
                    vec![
                        Instruction::ConstI(IntegerConstant::S32(0)),
                        Instruction::Ret(format::LenVec(vec![Register::Temporary(
                            TemporaryRegister::from(0)
                        )]))
                    ]
                );
                assert!(code.blocks.is_empty());
            }
        )
    }

    #[test]
    fn control_sample_compiles_successfully() {
        assert_success!(
            include_str!(r"../../../sailas/samples/control.txtmdl"),
            |module: format::Module| {
                let code = &module.function_bodies[0];
                assert_eq!(1, code.blocks.len());
                assert_eq!(
                    &Instruction::Br {
                        target: BlockIndex::from(1),
                        input_registers: format::LenVec(vec![Register::Temporary(
                            TemporaryRegister::from(1)
                        )])
                    },
                    code.entry_block.instructions.0.last().unwrap()
                );
            }
        )
    }
}
