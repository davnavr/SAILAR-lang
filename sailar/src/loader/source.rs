
use crate::loader::module::Record;

pub trait Source {
    type Content: std::iter::IntoIterator<Item = Record>;
    type Error;

    fn to_module_contents(self) -> Result<Self::Content, Self::Error>;
}

impl<E, I: std::iter::Iterator<Item = Result<Record, E>>> Source for I {
    type Content = Vec<Record>;
    type Error = Vec<E>;

    fn to_module_contents(self) -> Result<Self::Content, Self::Error> {
        let (minimum_size, maximum_size) = self.size_hint();
        let mut records = Vec::with_capacity(maximum_size.unwrap_or(minimum_size));
        let mut errors = Vec::default();

        for r in self {
            match r {
                Ok(r) => records.push(r),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty()
        { Ok(records) }
        else { Err(errors) }
    }
}
