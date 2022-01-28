#[derive(thiserror::Error, Clone, Debug)]
pub enum Error {}

pub type Result<T> = std::result::Result<T, Error>;
