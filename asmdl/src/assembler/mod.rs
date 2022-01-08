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
        block_lookup: &CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> format::CodeBlock {
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
            todo!("TODO: Implement assembling of instructions")
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

type IdentifierLookup = lookup::IndexedSet<format::indices::Identifier, format::Identifier>;
//type NamespaceLookup
//type TypeSignatureLookup
//type FunctionSignatureLookup

type CodeBlockLookup<'a> =
    lookup::IndexedMap<'a, u32, (&'a ast::Identifier, CodeBlockAssembler<'a>)>;

type FunctionBodyLookup<'a> =
    lookup::IndexedMap<'a, format::indices::Code, FunctionBodyAssembler<'a>>;

//type FunctionDefinitionLookup<'a> = lookup::IndexedMap<'a, format::indices::FunctionDefinition, FunctionDefinitionAssembler<'a>>;

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
    let mut function_bodies = FunctionBodyLookup::new();

    //let mut function_definitions = FunctionDefinitionLookup::new();

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
                        ErrorKind::DuplicateCode {
                            name: symbol.identifier().clone(),
                            original: error.location.clone(),
                        },
                        node.1.clone(),
                    );
                }
            }
            _ => todo!(),
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

            assemble_items(&function_bodies.into_vec(), |body| {
                body.assemble(&mut errors, &mut block_lookup, &mut register_map)
            })
        };

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
                defined_functions: format::LenVecBytes::from(Vec::new()),
            }),
            struct_layouts: format::LenVecBytes::from(Vec::new()),
            entry_point: format::LenBytes(None),
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
