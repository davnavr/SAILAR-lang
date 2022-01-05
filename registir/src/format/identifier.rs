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

impl TryFrom<&[char]> for Identifier {
    type Error = ();

    fn try_from(chars: &[char]) -> Result<Self, Self::Error> {
        if chars.is_empty() {
            Err(())
        } else {
            Ok(Self(chars.iter().collect()))
        }
    }
}

impl TryFrom<&str> for Identifier {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.is_empty() {
            Err(())
        } else {
            Ok(Self(String::from(s)))
        }
    }
}

impl std::ops::Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
