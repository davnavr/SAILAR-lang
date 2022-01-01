use crate::loader::{Identifier, ModuleIdentifier};
use registir::format;

pub trait FullIdentifier: Sized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;

    fn parse(s: &str) -> Option<Self>;
}

impl FullIdentifier for ModuleIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write as _;
        f.write_char('{')?;
        std::fmt::Display::fmt(&self.name, f)?;
        if !self.version.0.is_empty() {
            for (index, number) in self.version.0.iter().enumerate() {
                if index > 0 {
                    f.write_char('.')?;
                }
                std::fmt::Display::fmt(number, f)?;
            }
        }
        f.write_char('}')
    }

    fn parse(s: &str) -> Option<Self> {
        lazy_static::lazy_static!(
            static ref REGEX: regex::Regex = regex::Regex::new(r"\{([\w_]+)(,\s*(\d+(\.\d+)*))?\}").unwrap();
        );

        REGEX.captures(s).and_then(|ref captures| {
            Some(Self {
                name: Identifier::try_from(captures.get(1).unwrap().as_str()).ok()?,
                version: {
                    let mut numbers = Vec::new();
                    if let Some(version_numbers) = captures.get(3) {
                        for number in version_numbers
                            .as_str()
                            .split('.')
                            .filter(|s| !s.is_empty())
                        {
                            numbers.push(format::numeric::UInteger(number.parse().ok()?));
                        }
                    }
                    format::VersionNumbers(format::structures::LengthEncodedVector(numbers))
                },
            })
        })
    }
}

macro_rules! identifier_type_traits {
    ($implementor: ty) => {
        impl std::fmt::Display for $implementor {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                FullIdentifier::fmt(self, f)
            }
        }
    };
}

/// Identifiers and allows retrieval of a type definition.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FullTypeIdentifier {
    module_name: ModuleIdentifier,
    type_name: Identifier,
    //type_namespace: Vec<Identifier>, // TODO: Include namespace in full type identifier.
}

impl FullTypeIdentifier {
    pub fn new(module_name: ModuleIdentifier, type_name: Identifier) -> Self {
        Self {
            module_name,
            type_name,
        }
    }

    pub fn module_name(&self) -> &ModuleIdentifier {
        &self.module_name
    }

    pub fn type_name(&self) -> &Identifier {
        &self.type_name
    }
}

impl FullIdentifier for FullTypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.module_name().fmt(f)?;
        write!(f, "::{}", self.type_name)
    }

    fn parse(s: &str) -> Option<Self> {
        lazy_static::lazy_static!(
            static ref REGEX: regex::Regex = regex::Regex::new(r"::([\w_]+)").unwrap();
        );

        Some(Self {
            module_name: ModuleIdentifier::parse(s)?,
            type_name: Identifier::try_from(REGEX.captures(s)?.get(1)?.as_str()).ok()?,
        })
    }
}

identifier_type_traits!(FullTypeIdentifier);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FullMethodIdentifier {
    type_name: FullTypeIdentifier,
    method_name: Identifier,
    //parameter_types:
    //return_types:
}

impl FullMethodIdentifier {
    pub fn new(type_name: FullTypeIdentifier, method_name: Identifier) -> Self {
        Self {
            type_name,
            method_name,
        }
    }

    pub fn type_name(&self) -> &FullTypeIdentifier {
        &self.type_name
    }

    pub fn method_name(&self) -> &Identifier {
        &self.method_name
    }
}

impl FullIdentifier for FullMethodIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.type_name().fmt(f)?;
        write!(f, "::{}", self.method_name)
    }

    fn parse(s: &str) -> Option<Self> {
        lazy_static::lazy_static!(
            // a-zA-Z is hack until this doesn't match against version numbers.
            static ref REGEX: regex::Regex = regex::Regex::new(r"\.([a-zA-Z_]+)").unwrap();
        );

        Some(Self {
            type_name: FullTypeIdentifier::parse(s)?,
            method_name: Identifier::try_from(REGEX.captures(s)?.get(1)?.as_str()).ok()?,
        })
    }
}

identifier_type_traits!(FullMethodIdentifier);

#[cfg(test)]
mod tests {
    mod identifier {
        use crate::loader::names::{
            FullIdentifier as _, FullMethodIdentifier, FullTypeIdentifier, Identifier,
            ModuleIdentifier,
        };
        use registir::format::{
            numeric::UInteger, structures::LengthEncodedVector, VersionNumbers,
        };

        #[test]
        fn module_identifier_with_version_is_valid() {
            assert_eq!(
                ModuleIdentifier::parse("{abc, 1.2.34}"),
                Some(ModuleIdentifier {
                    name: Identifier::try_from("abc").unwrap(),
                    version: VersionNumbers(LengthEncodedVector(vec![
                        UInteger(1),
                        UInteger(2),
                        UInteger(34),
                    ]))
                })
            );
        }

        #[test]
        fn type_identifier_is_valid() {
            assert_eq!(
                FullTypeIdentifier::parse("{test, 1}::SomeTypeName"),
                Some(FullTypeIdentifier {
                    module_name: ModuleIdentifier {
                        name: Identifier::try_from("test").unwrap(),
                        version: VersionNumbers(LengthEncodedVector(vec![UInteger(1)]))
                    },
                    type_name: Identifier::try_from("SomeTypeName").unwrap(),
                })
            );
        }

        #[test]
        fn method_identifier_is_valid() {
            assert_eq!(
                FullMethodIdentifier::parse("{hello_world, 3.0.1}::Hello.Main"),
                Some(FullMethodIdentifier {
                    type_name: FullTypeIdentifier {
                        module_name: ModuleIdentifier {
                            name: Identifier::try_from("hello_world").unwrap(),
                            version: VersionNumbers(LengthEncodedVector(vec![
                                UInteger(3),
                                UInteger(0),
                                UInteger(1)
                            ]))
                        },
                        type_name: Identifier::try_from("Hello").unwrap(),
                    },
                    method_name: Identifier::try_from("Main").unwrap(),
                })
            );
        }
    }
}
