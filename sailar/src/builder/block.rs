use crate::format::instruction_set::{self, Instruction};
use crate::{builder, format};

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Register {
    code_index: format::indices::Code,
    block_index: format::indices::CodeBlock,
    index: format::indices::Register,
    value_type: builder::Type,
}

// Uses interior mutability, otherwise problems with borrowing occur if register references are used.
pub struct Block {
    owner_index: format::indices::Code,
    index: format::indices::CodeBlock,
    input_count: u32,
    expected_return_count: u32,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register>,
    instructions: std::cell::RefCell<Vec<Instruction>>,
}

impl Block {
    pub(super) fn new(
        owner_index: format::indices::Code,
        index: format::indices::CodeBlock,
        input_count: u32,
        expected_return_count: u32,
    ) -> Self {
        Self {
            owner_index,
            index,
            input_count,
            expected_return_count,
            register_index: builder::counter::Cell::new(),
            registers: typed_arena::Arena::new(),
            instructions: std::cell::RefCell::new(Vec::new()),
        }
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&self) -> u32 {
        self.expected_return_count
    }

    fn allocate_register(&self, value_type: builder::Type) -> &Register {
        self.registers.alloc(Register {
            code_index: self.owner_index,
            block_index: self.index,
            index: format::indices::Register::Temporary(self.register_index.next()),
            value_type,
        })
    }

    pub fn reserve_registers(&self, count: usize) {
        self.registers.reserve_extend(count)
    }

    pub fn emit_raw(&self, instruction: Instruction) {
        self.instructions.borrow_mut().push(instruction);
    }

    pub fn nop(&self) {
        self.emit_raw(Instruction::Nop);
    }

    pub fn const_i<C: Into<instruction_set::IntegerConstant>>(&self, constant: C) -> &Register {
        let value = constant.into();
        let value_type = value.value_type();
        self.emit_raw(Instruction::ConstI(value));
        self.allocate_register(format::type_system::Any::Primitive(
            format::type_system::Primitive::from(value_type),
        ))
    }

    pub fn ret<'b, R: std::iter::IntoIterator<Item = &'b Register>>(
        &'b self,
        results: R,
    ) -> Result<()> {
        let result_indices = results
            .into_iter()
            .map(|register| register.index)
            .collect::<Vec<_>>();

        let actual_count = result_indices.len().try_into().unwrap();

        self.emit_raw(Instruction::Ret(format::LenVec(result_indices)));

        if actual_count == self.expected_return_count {
            Ok(())
        } else {
            Err(Error::ResultCountMismatch {
                expected: self.expected_return_count,
                actual: actual_count,
            })
        }
    }

    pub(super) fn build(&mut self) -> format::CodeBlock {
        format::CodeBlock {
            input_register_count: format::numeric::UInteger(self.input_count),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(self.instructions.take())),
        }
    }
}
