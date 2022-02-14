use crate::loader::{self, cache, Error, Register, Result};
use sailar::format;

pub struct JumpTarget<'a> {
    destination: &'a Block<'a>,
    inputs: Box<[&'a Register<'a>]>,
}

impl<'a> JumpTarget<'a> {
    pub fn destination(&'a self) -> &'a Block<'a> {
        self.destination
    }

    pub fn inputs(&'a self) -> &'a [&'a Register<'a>] {
        &self.inputs
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
    jump_targets: cache::Once<Vec<JumpTarget<'a>>>,
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
            jump_targets: cache::Once::new(),
        }
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.code.declaring_module()
    }

    pub fn declaring_code(&'a self) -> &'a Code<'a> {
        self.code
    }

    pub fn index(&'a self) -> format::indices::CodeBlock {
        self.index
    }

    pub fn as_raw(&'a self) -> &'a format::CodeBlock {
        self.source
    }

    pub fn raw_instructions(&'a self) -> &'a [format::instruction_set::Instruction] {
        &self.source.instructions.0 .0
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
                types.push(reg.value_type());
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

    pub fn register_at(&'a self, index: format::indices::Register) -> Result<&'a Register<'a>> {
        todo!()
    }

    /// Returns all possible blocks that control can transfer to when the current block is executed, as well as the registers
    /// used as inputs to the target block.
    pub fn jump_targets(&'a self) -> Result<&'a [JumpTarget<'a>]> {
        self.jump_targets.get_or_insert_fallible(|| {
            use format::instruction_set::Instruction;

            let mut targets = Vec::new();

            let push_targets = |target: format::indices::CodeBlock, input_indices: &'a [format::indices::Register]| -> Result<()> {
                let destination = self.code.load_block(target)?;

                // TODO: Could check that input count and types matches target block.
                let mut inputs = Vec::with_capacity(input_indices.len());
                for index in input_indices.iter() {
                    inputs.push(self.register_at(*index)?);
                }

                targets.push(JumpTarget {
                    destination,
                    inputs: inputs.into_boxed_slice(),
                });

                Ok(())
            };

            for instruction in self.raw_instructions().rev() {
                match instruction {
                    //Instruction::Switch
                    Instruction::Br { target, input_registers: input_indices } => {
                        push_targets(target, &input_indices.0)?;
                        break;
                    }
                    _ => (),
                }
            }

            Ok(targets)
        }).map(|targets| targets as &'a [_])
    }
}

pub struct Code<'a> {
    source: &'a format::Code,
    module: &'a loader::Module<'a>,
    blocks: Box<[cache::Once<Block<'a>>]>,
    all_blocks: cache::Once<Box<[&'a Block<'a>]>>,
}

impl<'a> Code<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::Code) -> Self {
        Self {
            module,
            source,
            blocks: {
                let mut blocks = Vec::new();
                blocks.resize_with(1 + source.blocks.len(), cache::Once::new);
                blocks.into_boxed_slice()
            },
            all_blocks: cache::Once::new(),
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

    pub fn all_blocks(&'a self) -> Result<&'a [&'a Block<'a>]> {
        self.all_blocks.get_or_insert_fallible(|| {
            let mut blocks = Vec::with_capacity(self.blocks.len());
            for index in 0..self.blocks.len() {
                blocks.push(self.load_block(format::indices::CodeBlock::try_from(index).unwrap())?);
            }
            Ok(blocks.into_boxed_slice())
        }).map(|blocks| blocks as &'a [_])
    }

    pub fn input_types(&'a self) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        self.entry_block().input_types()
    }
}
