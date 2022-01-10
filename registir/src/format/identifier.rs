use std::fmt::{Debug, Display, Formatter};

/// Represents a length-encoded UTF-8 string that cannot be empty.
#[derive(Clone, Eq, Hash, PartialEq, PartialOrd)]
pub struct Identifier(String);

impl Identifier {
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

macro_rules! identifier_conversion_from {
    ($source_type: ty, $source: ident, $conversion: expr) => {
        impl TryFrom<$source_type> for Identifier {
            type Error = ();

            fn try_from($source: $source_type) -> Result<Self, Self::Error> {
                if $source.is_empty() {
                    Err(())
                } else {
                    Ok(Self($conversion))
                }
            }
        }
    };
}

identifier_conversion_from!(&[char], chars, chars.iter().collect());
identifier_conversion_from!(&str, s, String::from(s));
identifier_conversion_from!(String, s, s);

impl std::ops::Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
