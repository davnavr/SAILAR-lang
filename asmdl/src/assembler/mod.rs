use crate::ast;
use registir::format;

mod declare;
mod indexed;

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Declaration {
    Code,
    TypeDefinition,
    Module,
    Format,
    Name,
    /// A module or format version.
    Version,
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
    DuplicateNamedDeclaration(ast::GlobalSymbol, Declaration),
    DuplicateDeclaration(ast::Position, Declaration),
    InvalidNameDeclaration(ast::Position, Declaration, NameError),
    MissingDeclaration(Option<ast::Position>, Declaration),
}

impl Error {
    pub fn position(&self) -> Option<&ast::Position> {
        match self {
            Self::DuplicateNamedDeclaration(
                ast::GlobalSymbol(ast::Positioned { position, .. }),
                _,
            )
            | Self::DuplicateDeclaration(position, _)
            | Self::InvalidNameDeclaration(position, _, _) => Some(position),
            Self::MissingDeclaration(position, _) => position.as_ref(),
        }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Name => "name",
            Self::TypeDefinition => "type definition",
            Self::Code => "code",
            Self::Module => "module",
            Self::Format => "format",
            Self::Version => "version",
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
            Self::DuplicateDeclaration(_, declaration) => {
                write!(f, "a {} declaration already exists", declaration)
            }
            Self::InvalidNameDeclaration(_, declaration, error) => {
                write!(f, "{} name for {} declaration", error, declaration)
            }
            Self::MissingDeclaration(_, declaration) => {
                write!(f, "missing {} declaration", declaration)
            }
        }
    }
}

fn assemble_module_header(
    errors: &mut Vec<Error>,
    default_module_name: &format::Identifier,
    declarations: &[ast::Positioned<ast::ModuleDeclaration>],
) -> format::ModuleHeader {
    let mut module_name = declare::Once::new(|name: &ast::Positioned<_>, _| {
        Error::InvalidNameDeclaration(name.position, Declaration::Module, NameError::Duplicate)
    });
    let mut module_version = declare::Once::new(|version: ast::Positioned<_>, _| {
        Error::DuplicateDeclaration(version.position, Declaration::Version)
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
                                Declaration::Module,
                                NameError::Empty,
                            ));
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
    fn declare_version<'a>() -> declare::Once<'a, ast::Position, format::uvarint, impl Fn(ast::Position, &format::uvarint) -> Error> {
        declare::Once::new(|position, _| Error::DuplicateDeclaration(position, Declaration::Version))
    }

    let mut major_version = declare_version();
    let mut minor_version = declare_version();

    for node in declarations {
        match &node.value {
            ast::FormatDeclaration::Major(major) => if let Some(set_version) = major_version.declare(errors, node.position) {
                set_version(*major)
            },
            ast::FormatDeclaration::Minor(minor) => if let Some(set_version) = minor_version.declare(errors, node.position) {
                set_version(*minor)
            },
        }
    }

    format::FormatVersion {
        major: major_version.value().unwrap_or_default(),
        minor: minor_version.value().unwrap_or_default(),
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
        //let mut type_name = declare::Once
        let mut visibility =
            declare::Once::new(|modifier: &ast::Positioned<_>, _| unimplemented!());
        let mut flags = format::TypeDefinitionFlags::default();

        for declaration in self.declarations {
            match &declaration.value {
                _ => unimplemented!(),
            }
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
                    errors.push(Error::DuplicateDeclaration(node.position, Declaration::Module))
                }
            }
            ast::TopLevelDeclaration::Format(ref format_versions) => match module_format {
                None => module_format = Some(assemble_module_format(&mut errors, format_versions)),
                Some(_) => errors.push(Error::DuplicateDeclaration(node.position, Declaration::Format)),
            },
            ast::TopLevelDeclaration::Code {
                ref symbol,
                declarations,
            } => match method_bodies.try_add(symbol, declarations) {
                Some(_) => (),
                None => errors.push(Error::DuplicateNamedDeclaration(symbol.clone(), Declaration::Code)),
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
                None => errors.push(Error::DuplicateNamedDeclaration(symbol.clone(), Declaration::TypeDefinition)),
            },
            ref unknown => unimplemented!("{:?}", unknown),
        }
    }

    if module_header.is_none() {
        errors.push(Error::MissingDeclaration(None, Declaration::Module))
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
