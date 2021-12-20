use crate::ast;
use registir::format;

mod declare;
mod indexed;

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum NameError {
    Duplicate,
    Empty,
    //Missing,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    DuplicateCodeDeclaration(ast::GlobalSymbol),
    DuplicateTypeDefinitionDeclaration(ast::GlobalSymbol),
    DuplicateFormatDeclaration(ast::Position),
    DuplicateMajorVersion(ast::Position),
    DuplicateMinorVersion(ast::Position),
    DuplicateModuleDeclaration(ast::Position),
    DuplicateModuleVersion(ast::Position),
    InvalidModuleName(ast::Position, NameError),
    MissingModuleDeclaration,
}

impl Error {
    pub fn position(&self) -> Option<ast::Position> {
        match self {
            Self::DuplicateCodeDeclaration(ast::GlobalSymbol(ast::Positioned {
                position, ..
            }))
            | Self::DuplicateTypeDefinitionDeclaration(ast::GlobalSymbol(ast::Positioned {
                position,
                ..
            }))
            | Self::DuplicateFormatDeclaration(position)
            | Self::DuplicateMajorVersion(position)
            | Self::DuplicateMinorVersion(position)
            | Self::DuplicateModuleDeclaration(position)
            | Self::DuplicateModuleVersion(position)
            | Self::InvalidModuleName(position, _) => Some(*position),
            Self::MissingModuleDeclaration => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateCodeDeclaration(ast::GlobalSymbol(ast::Positioned {
                value: name,
                ..
            })) => {
                write!(
                    f,
                    "a code declaration with the name @{} was already defined",
                    name
                )
            }
            Self::DuplicateTypeDefinitionDeclaration(ast::GlobalSymbol(ast::Positioned {
                value: name,
                ..
            })) => {
                write!(
                    f,
                    "a type definition with the name @{} was already declared",
                    name
                )
            }
            Self::DuplicateFormatDeclaration(_) => {
                f.write_str("the module format was already specified")
            }
            Self::DuplicateMajorVersion(_) => {
                f.write_str("the module format major version was already specified")
            }
            Self::DuplicateMinorVersion(_) => {
                f.write_str("the module format minor version was already specified")
            }
            Self::DuplicateModuleDeclaration(_) => {
                f.write_str("a module declaration already exists")
            }
            Self::DuplicateModuleVersion(_) => {
                f.write_str("the module version was already declared")
            }
            Self::InvalidModuleName(_, NameError::Duplicate) => {
                write!(f, "the module name was already declared")
            }
            Self::InvalidModuleName(_, NameError::Empty) => {
                write!(f, "the module name cannot be empty")
            }
            Self::MissingModuleDeclaration => f.write_str(
                "missing module declaration, declare a module with the `.module` directive",
            ),
        }
    }
}

fn assemble_module_header(
    errors: &mut Vec<Error>,
    default_module_name: &format::Identifier,
    declarations: &[ast::Positioned<ast::ModuleDeclaration>],
) -> format::ModuleHeader {
    let mut module_name = declare::Once::new(|name: &ast::Positioned<_>, _| {
        Error::InvalidModuleName(name.position, NameError::Duplicate)
    });
    let mut module_version = declare::Once::new(|version: ast::Positioned<_>, _| {
        Error::DuplicateModuleVersion(version.position)
    });

    for node in declarations {
        match &node.value {
            ast::ModuleDeclaration::Name(name) => {
                if let Some(set_name) = module_name.declare(errors, name) {
                    set_name(match format::Identifier::try_from(&name.value.0) {
                        Ok(id) => Some(id),
                        Err(_) => {
                            errors.push(Error::InvalidModuleName(name.position, NameError::Empty));
                            None
                        }
                    })
                }
            }
            ast::ModuleDeclaration::Version(version) => {
                if let Some(set_version) = module_version.declare(errors, node.map(|_| version)) {
                    set_version(format::VersionNumbers(format::LengthEncodedVector(
                        version.iter().map(|n| format::uvarint(*n)).collect(),
                    )))
                }
            }
        }
    }

    match module_name.value().flatten() {
        Some(name) => format::ModuleHeader {
            identifier: format::ModuleIdentifier {
                name,
                version: module_version.value().unwrap_or_default(),
            },
        },
    }
}

fn assemble_module_format(
    errors: &mut Vec<Error>,
    declarations: &[ast::Positioned<ast::FormatDeclaration>],
) -> format::FormatVersion {
    let mut major_version = None;
    let mut minor_version = None;

    for node in declarations {
        match &node.value {
            ast::FormatDeclaration::Major(major) => match major_version {
                None => major_version = Some(*major),
                Some(_) => errors.push(Error::DuplicateMajorVersion(node.position)),
            },
            ast::FormatDeclaration::Minor(minor) => match minor_version {
                None => minor_version = Some(*minor),
                Some(_) => errors.push(Error::DuplicateMinorVersion(node.position)),
            },
        }
    }

    format::FormatVersion {
        major: major_version.unwrap_or_default(),
        minor: minor_version.unwrap_or_default(),
    }
}

type IdentifierLookup = indexed::Set<format::IdentifierIndex, format::Identifier>;

#[derive(Debug)]
struct TypeDefinitionAssembler<'a> {
    modifiers: &'a [ast::Positioned<ast::TypeModifier>],
    declarations: &'a [ast::Positioned<ast::TypeDeclaration>],
}

impl<'a> TypeDefinitionAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut Vec<Error>,
        identifiers: &IdentifierLookup,
    ) -> format::TypeDefinition {
        let mut visibility = None;
        let mut flags = format::TypeDefinitionFlags::default();

        for declaration in self.declarations {
            match &declaration.value {}
        }

        format::TypeDefinition {
            visibility: visibility.unwrap_or_default(),
            flags,
        }
    }
}

pub fn assemble_declarations(
    declarations: &[ast::Positioned<ast::TopLevelDeclaration>],
    default_module_name: format::Identifier, // TODO: Remove default module name and just add an error for missing module name.
) -> Result<format::Module, Vec<Error>> {
    let mut errors = Vec::new();
    let mut module_header = None;
    let mut module_format = None;
    let mut identifiers = IdentifierLookup::new();
    let mut method_bodies = indexed::SymbolMap::<ast::GlobalSymbol, format::CodeIndex, _>::new();
    let mut type_definitions =
        indexed::SymbolMap::<ast::GlobalSymbol, usize, TypeDefinitionAssembler<'_>>::new();

    for node in declarations {
        match &node.value {
            ast::TopLevelDeclaration::Module(ref module_nodes) => {
                if module_header.is_none() {
                    module_header = Some(assemble_module_header(
                        &mut errors,
                        &default_module_name,
                        module_nodes,
                    ))
                } else {
                    errors.push(Error::DuplicateModuleDeclaration(node.position))
                }
            }
            ast::TopLevelDeclaration::Format(ref format_versions) => match module_format {
                None => module_format = Some(assemble_module_format(&mut errors, format_versions)),
                Some(_) => errors.push(Error::DuplicateFormatDeclaration(node.position)),
            },
            ast::TopLevelDeclaration::Code {
                ref symbol,
                declarations,
            } => match method_bodies.try_add(symbol, declarations) {
                Some(_) => (),
                None => errors.push(Error::DuplicateCodeDeclaration(symbol.clone())),
            },
            ast::TopLevelDeclaration::Type {
                ref symbol,
                modifiers,
                declarations,
            } => match type_definitions.try_add(
                symbol,
                TypeDefinitionAssembler {
                    modifiers,
                    declarations,
                },
            ) {
                Some(_) => (),
                None => errors.push(Error::DuplicateTypeDefinitionDeclaration(symbol.clone())),
            },
            ref unknown => unimplemented!("{:?}", unknown),
        }
    }

    if module_header.is_none() {
        errors.push(Error::MissingModuleDeclaration)
    }

    if errors.is_empty() {
        Ok(format::Module {
            format_version: module_format.unwrap_or_default(),
            header: format::ByteLengthEncoded(module_header.unwrap()),
            // TODO: Add other things
            identifiers: format::ByteLengthEncoded(format::LengthEncodedVector::default()),
            namespaces: format::ByteLengthEncoded(format::LengthEncodedVector::default()),
            type_signatures: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
            method_signatures: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
            method_bodies: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
            data_arrays: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
            imports: format::ByteLengthEncoded(format::ModuleImports {
                imported_modules: format::LengthEncodedVector(Vec::new()),
                imported_types: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
                imported_fields: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
                imported_methods: format::ByteLengthEncoded(
                    format::LengthEncodedVector(Vec::new()),
                ),
            }),
            definitions: format::ByteLengthEncoded(format::ModuleDefinitions {
                defined_types: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
                defined_fields: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
                defined_methods: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
            }),
            entry_point: format::ByteLengthEncoded(None),
            type_layouts: format::ByteLengthEncoded(format::LengthEncodedVector(Vec::new())),
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
        let header = assembler::assemble_declarations(
            &declarations,
            format::Identifier::try_from("a").unwrap(),
        )
        .map(|module| module.header.0)
        .unwrap();

        assert_eq!(
            Ok(header.identifier.name),
            format::Identifier::try_from("Test")
        );
        assert_eq!(
            header.identifier.version,
            format::VersionNumbers::from_iter(vec![1u64, 2, 3].into_iter())
        );
    }
}
