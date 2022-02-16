use crate::loader::cache::Once;
use crate::loader::{self, Error, Register, Result};
use sailar::format;

pub enum InputSource<'a> {
    Callee,
    Block {
        block: &'a Block<'a>,
        registers: &'a [&'a loader::Register<'a>],
    },
}

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
    input_types: Once<Box<[&'a loader::TypeSignature<'a>]>>,
    input_registers: Once<Box<[Register<'a>]>>,
    temporary_types: Once<Box<[&'a loader::TypeSignature<'a>]>>,
    temporary_registers: Once<Box<[Register<'a>]>>,
    jump_targets: Once<Vec<JumpTarget<'a>>>,
    input_sources: Once<Vec<InputSource<'a>>>,
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
            input_types: Once::new(),
            input_registers: Once::new(),
            temporary_types: Once::new(),
            temporary_registers: Once::new(),
            jump_targets: Once::new(),
            input_sources: Once::new(),
        }
    }

    pub fn is_entry_block(&'a self) -> bool {
        self.index.0 .0 == 0u32
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
        cache: &'a Once<Box<[Register<'a>]>>,
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
        cache: &'a Once<Box<[&'a loader::TypeSignature<'a>]>>,
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

    // TODO: Return input register slice?
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
        match index {
            format::indices::Register::Input(input_index) => {
                loader::read_index_from(input_index, self.input_registers()?, Ok)
            }
            format::indices::Register::Temporary(temporary_index) => {
                loader::read_index_from(temporary_index, self.temporary_registers()?, Ok)
            }
        }
    }

    /// Returns all possible blocks that control can transfer to when the current block is executed, as well as the registers
    /// used as inputs to the target block.
    pub fn jump_targets(&'a self) -> Result<&'a [JumpTarget<'a>]> {
        self.jump_targets
            .get_or_insert_fallible(|| {
                use format::instruction_set::Instruction;

                let mut targets = Vec::new();

                let mut push_targets = |target: format::indices::CodeBlock,
                                        input_indices: &'a [format::indices::Register]|
                 -> Result<()> {
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

                for instruction in self.raw_instructions().iter().rev() {
                    match instruction {
                        Instruction::Switch { .. } => {
                            todo!("switch is not yet supported in jump target calculations")
                        }
                        Instruction::Br {
                            target,
                            input_registers,
                        } => {
                            push_targets(*target, &input_registers.0)?;
                            break;
                        }
                        Instruction::BrIf {
                            true_branch,
                            true_inputs,
                            false_branch,
                            false_inputs,
                            ..
                        } => {
                            push_targets(*true_branch, &true_inputs.0)?;
                            push_targets(*false_branch, &false_inputs.0)?;
                            break;
                        }
                        _ => (),
                    }
                }

                Ok(targets)
            })
            .map(Vec::as_slice)
    }

    /// Returns all blocks that transfer control to the current block, along with the registers used as inputs to the current
    /// block.
    pub fn input_sources(&'a self) -> Result<&'a [InputSource<'a>]> {
        self.input_sources
            .get_or_insert_fallible(|| {
                let mut sources = Vec::new();

                if self.is_entry_block() {
                    sources.push(InputSource::Callee);
                }

                for other_block in self.code.all_blocks()?.iter() {
                    for target in other_block.jump_targets()? {
                        if std::ptr::eq(self, target.destination()) {
                            sources.push(InputSource::Block {
                                block: other_block,
                                registers: target.inputs(),
                            });
                        }
                    }
                }

                Ok(sources)
            })
            .map(Vec::as_slice)
    }
}

pub struct Code<'a> {
    source: &'a format::Code,
    module: &'a loader::Module<'a>,
    blocks: Box<[Once<Block<'a>>]>,
    all_blocks: Once<Box<[&'a Block<'a>]>>,
}

impl<'a> Code<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::Code) -> Self {
        Self {
            module,
            source,
            blocks: {
                let mut blocks = Vec::new();
                blocks.resize_with(1 + source.blocks.len(), Once::new);
                blocks.into_boxed_slice()
            },
            all_blocks: Once::new(),
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
        self.all_blocks
            .get_or_insert_fallible(|| {
                let mut blocks = Vec::with_capacity(self.blocks.len());
                for index in 0..self.blocks.len() {
                    blocks.push(
                        self.load_block(format::indices::CodeBlock::try_from(index).unwrap())?,
                    );
                }
                Ok(blocks.into_boxed_slice())
            })
            .map(|blocks| blocks as &'a [_])
    }

    pub fn input_types(&'a self) -> Result<&'a [&'a loader::TypeSignature<'a>]> {
        self.entry_block().input_types()
    }
}
