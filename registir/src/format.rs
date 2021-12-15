/// The magic number for `binmdl` files.
pub static MAGIC: &'static [u8] = "reg\0".as_bytes();

/// Represents a variable-length unsigned integer.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
#[allow(non_camel_case_types)]
pub struct uvarint(pub u64);

/// Represents a variable-length signed integer.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
#[allow(non_camel_case_types)]
pub struct varint(pub u128);

pub trait Index: Copy {
    fn index(self) -> uvarint;
}

/// An index into the module's identifiers, the index of the first identifier is `0`.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct IdentifierIndex(pub uvarint);

impl Index for IdentifierIndex {
    fn index(self) -> uvarint {
        let IdentifierIndex(index) = self;
        index
    }
}

/// Represents data that is preceded by a variable-length unsigned integer indicating the byte length of the following data.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct ByteLengthEncoded<T>(pub T);

/// Represents an array preceded by an variable-length unsigned integer indicating the number of items.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct LengthEncodedVector<T>(pub Vec<T>);

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
#[derive(Debug, Default)]
pub struct VersionNumbers(pub LengthEncodedVector<uvarint>);

/// Describes the features that a module makes use of.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct FormatVersion {
    pub major: uvarint,
    pub minor: uvarint,
}

/// Represents a length-encoded UTF-8 string that cannot be empty.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct Identifier(LengthEncodedVector<u8>);

impl Identifier {
    pub fn with_bytes(bytes: Vec<u8>) -> Identifier {
        Identifier(LengthEncodedVector(bytes))
    }

    pub fn bytes(&self) -> &Vec<u8> {
        let Identifier(LengthEncodedVector(bytes)) = self;
        bytes
    }
}

#[derive(Debug)]
pub struct ModuleIdentifier {
    pub name: Identifier,
    pub version: VersionNumbers,
}

pub static MAX_HEADER_FIELD_COUNT: uvarint = uvarint(1);

#[derive(Debug)]
pub struct ModuleHeader {
    //pub field_count: (),
    pub identifier: ModuleIdentifier,
}

impl ModuleHeader {
    /// Variable-length unsigned integer placed at the start of the header indicating the number of fields present.
    pub fn field_count(&self) -> uvarint { MAX_HEADER_FIELD_COUNT }
}

pub type ModuleData<T> = Option<ByteLengthEncoded<T>>;

/// Represents the contents of a `binmdl` file following the [`MAGIC`] number.
#[derive(Debug)]
pub struct Module {
    pub format_version: FormatVersion,
    //pub data_count: (),
    /// The header, which identifies and describes the module.
    pub header: ModuleData<ModuleHeader>,
    /// An array containing the names of the types, namespaces, fields, and methods.
    pub identifiers: ModuleData<LengthEncodedVector<Identifier>>,
    /// An array of the namespaces containing the imported and defined types.
    pub namespaces: ModuleData<LengthEncodedVector<LengthEncodedVector<IdentifierIndex>>>,
}

impl Module {
    /// Variable-length unsigned integer following the format version indicating the number of length encoded things to follow.
    pub fn data_count(&self) -> uvarint {
        macro_rules! count_data {
            ($field: ident) => { self.$field.is_some() as u64 };
        }

        uvarint(
            count_data!(header) +
            count_data!(identifiers) +
            count_data!(namespaces)
        )
    }
}
