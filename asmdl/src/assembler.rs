use crate::ast;
use registir::format;

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum NameError {
    Duplicate,
    Empty,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    DuplicateModuleDeclaration(ast::Position),
    DuplicateModuleVersion(ast::Position),
    InvalidModuleName(ast::Position, NameError),
    MissingModuleDeclaration,
}

fn assemble_module_header(
    errors: &mut Vec<Error>,
    default_module_name: &format::Identifier,
    declarations: &Vec<ast::Positioned<ast::ModuleDeclaration>>,
) -> format::ModuleHeader {
    let mut module_name = None;
    let mut module_version = None;

    for node in declarations {
        match &node.value {
            ast::ModuleDeclaration::Name(name) => {
                if module_name.is_none() {
                    match format::Identifier::try_from(&name.value.0) {
                        Ok(id) => module_name = Some(id),
                        Err(_) => {
                            errors.push(Error::InvalidModuleName(name.position, NameError::Empty))
                        }
                    }
                } else {
                    errors.push(Error::InvalidModuleName(
                        node.position,
                        NameError::Duplicate,
                    ))
                }
            }
            ast::ModuleDeclaration::Version(version) => match module_version {
                None => {
                    module_version = Some(format::VersionNumbers(format::LengthEncodedVector(
                        version.iter().map(|n| format::uvarint(*n)).collect(),
                    )))
                }
                Some(_) => errors.push(Error::DuplicateModuleVersion(node.position)),
            },
        }
    }

    format::ModuleHeader {
        identifier: format::ModuleIdentifier {
            name: module_name.unwrap_or(format::Identifier::clone(default_module_name)),
            version: module_version.unwrap_or(format::VersionNumbers::default()),
        },
    }
}

pub fn assemble_declarations(
    declarations: &[ast::Positioned<ast::TopLevelDeclaration>],
    default_module_name: format::Identifier,
) -> Result<format::Module, Vec<Error>> {
    let mut errors = Vec::new();
    let mut module_header = None;
    let mut module_format = None;

    for node in declarations {
        match node.value {
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
            _ => unimplemented!(),
        }
    }

    if module_header.is_none() {
        errors.push(Error::MissingModuleDeclaration)
    }

    if errors.is_empty() {
        Ok(format::Module {
            format_version: module_format.unwrap_or(format::FormatVersion::default()),
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
