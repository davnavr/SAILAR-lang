use crate::ast;
use registir::format;

mod error;

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

pub fn assemble_declarations(
    declarations: &[ast::Positioned<ast::TopLevelDeclaration>],
) -> Result<format::Module, Vec<Error>> {
    let mut errors = error::Builder::new();
    let mut module_header = None;
    let mut module_format = None;

    for node in declarations {
        match &node.0 {
            ast::TopLevelDeclaration::Module(declarations) => {
                assemble_module_header(&mut errors, &mut module_header, &declarations, &node.1)
            }
            ast::TopLevelDeclaration::Format(declarations) => {
                assemble_module_format(&mut errors, &mut module_format, &declarations, &node.1)
            }
            _ => todo!(),
        }
    }

    todo!()
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
