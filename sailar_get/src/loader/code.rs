use crate::loader::{self, cache, Error, Result};
use sailar::format;

pub struct Register<'a> {
    block: &'a Block<'a>,
    index: format::indices::Register,
    value_type: &'a loader::TypeSignature<'a>,
}

impl<'a> Register<'a> {
    fn new(
        block: &'a Block<'a>,
        index: format::indices::Register,
        value_type: &'a loader::TypeSignature<'a>,
    ) -> Self {
        Self {
            block,
            index,
            value_type,
        }
    }

    pub fn block(&'a self) -> &'a Block<'a> {
        self.block
    }

    pub fn index(&'a self) -> format::indices::Register {
        self.index
    }

    pub fn is_temporary(&'a self) -> bool {
        match self.index {
            format::indices::Register::Temporary(_) => true,
            format::indices::Register::Input(_) => false,
        }
    }
}

pub struct Block<'a> {
    source: &'a format::CodeBlock,
    code: &'a Code<'a>,
    index: format::indices::CodeBlock,
    input_types: cache::Once<Box<[&'a loader::TypeSignature<'a>]>>,
    input_registers: cache::Once<Box<[Register<'a>]>>,
    temporary_types: cache::Once<Box<[&'a loader::TypeSignature<'a>]>>,
    temporary_registers: cache::Once<Box<[Register<'a>]>>,
}

impl<'a> Block<'a> {
    fn new(
        source: &'a format::CodeBlock,
        code: &'a Code<'a>,
        index: format::indices::CodeBlock,
    ) -> Self {
        Self {
            source,
            code,
            index,
            input_types: cache::Once::new(),
            input_registers: cache::Once::new(),
            temporary_types: cache::Once::new(),
            temporary_registers: cache::Once::new(),
        }
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.code.declaring_module()
    }

    pub fn index(&'a self) -> format::indices::CodeBlock {
        self.index
    }

    pub fn as_raw(&'a self) -> &'a format::CodeBlock {
        self.source
    }

    fn registers<I: Fn(u32) -> format::indices::Register>(
        &'a self,
        cache: &'a cache::Once<Box<[Register<'a>]>>,
        types: &'a [format::indices::TypeSignature],
        register_index: I,
    ) -> Result<&'a [Register<'a>]> {
        let registers = cache.get_or_insert_fallible(|| -> Result<_> {
            let mut registers = Vec::with_capacity(types.len());
            for (type_index, index) in types.iter().copied().zip(0u32..) {
                registers.push(Register::new(
                    self,
                    register_index(index),
                    self.declaring_module().load_type_signature(type_index)?,
                ))
            }
            Ok(registers.into_boxed_slice())
        })?;
        Ok(registers)
    }

    fn register_types<R: FnOnce() -> Result<&'a [Register<'a>]>>(
        cache: &'a cache::Once<Box<[&'a loader::TypeSignature<'a>]>>,
        registers: R,
    ) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        let types = cache.get_or_insert_fallible(|| -> Result<_> {
            let registers = registers()?;
            let mut types = Vec::with_capacity(registers.len());
            for reg in registers.iter() {
                types.push(reg.value_type);
            }
            Ok(types.into_boxed_slice())
        })?;
        Ok(types)
    }

    pub fn input_registers(&'a self) -> Result<&'a [Register<'a>]> {
        self.registers(
            &self.input_registers,
            &self.source.input_registers.0,
            |index| format::indices::Register::Input(format::indices::InputRegister::from(index)),
        )
    }

    pub fn input_types(&'a self) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        Self::register_types(&self.input_types, || self.input_registers())
    }

    pub fn temporary_registers(&'a self) -> Result<&'a [Register<'a>]> {
        self.registers(
            &self.temporary_registers,
            &self.source.temporary_registers.0,
            |index| {
                format::indices::Register::Temporary(format::indices::TemporaryRegister::from(
                    index,
                ))
            },
        )
    }

    pub fn temporary_types(&'a self) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        Self::register_types(&self.temporary_types, || self.temporary_registers())
    }
}

pub struct Code<'a> {
    source: &'a format::Code,
    module: &'a loader::Module<'a>,
    blocks: Vec<cache::Once<Block<'a>>>,
}

impl<'a> Code<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::Code) -> Self {
        Self {
            module,
            source,
            blocks: {
                let mut blocks = Vec::new();
                blocks.resize_with(1 + source.blocks.len(), cache::Once::new);
                blocks
            },
        }
    }

    pub fn as_raw(&'a self) -> &'a format::Code {
        self.source
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.module
    }

    pub fn load_block(&'a self, index: format::indices::CodeBlock) -> Result<&'a Block<'a>> {
        loader::read_index(index, |raw_index| match self.blocks.get(raw_index) {
            Some(entry) => entry.get_or_insert_fallible(|| {
                let source = if raw_index == 0 {
                    &self.source.entry_block
                } else {
                    &self.source.blocks[raw_index - 1]
                };

                Ok(Block::new(source, self, index))
            }),
            None => Err(Error::IndexOutOfBounds(index.into())),
        })
    }

    pub fn entry_block(&'a self) -> &'a Block<'a> {
        self.load_block(format::indices::CodeBlock::from(0u32))
            .unwrap()
    }

    pub fn input_types(&'a self) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        self.entry_block().input_types()
    }
}
