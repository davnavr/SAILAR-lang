/// The magic number for `binmdl` files.
pub static MAGIC: &'static [u8] = "reg\0".as_bytes();

#[derive(Debug)]
pub struct Module {
    pub major_version: u8,
    pub minor_version: u8,
}
