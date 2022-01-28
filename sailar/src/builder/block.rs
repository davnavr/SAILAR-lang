use crate::format::instruction_set::{self, Instruction};
use crate::{builder, format};

pub struct Register {
    code_index: format::indices::Code,
    block_index: format::indices::CodeBlock,
    index: format::indices::Register,
    value_type: builder::Type,
}

pub struct Block {
    owner_index: format::indices::Code,
    index: format::indices::CodeBlock,
    expected_return_count: u32,
    register_index: builder::counter::Counter<format::indices::TemporaryRegister>,
    registers: Vec<Box<Register>>,
    instructions: Vec<Instruction>,
}

impl Block {
    pub(super) fn new(
        owner_index: format::indices::Code,
        index: format::indices::CodeBlock,
        expected_return_count: u32,
    ) -> Self {
        Self {
            owner_index,
            index,
            expected_return_count,
            register_index: builder::counter::Counter::new(),
            registers: Vec::new(),
            instructions: Vec::new(),
        }
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&self) -> u32 {
        self.expected_return_count
    }

    fn allocate_register(&mut self, value_type: builder::Type) -> &Register {
        let index = self.registers.len();
        let register = Box::new(Register {
            code_index: self.owner_index,
            block_index: self.index,
            index: format::indices::Register::Temporary(self.register_index.next()),
            value_type,
        });

        self.registers.push(register);

        // The index is guaranteed to point to the newly added register.
        unsafe { std::borrow::Borrow::borrow(self.registers.get_unchecked(index)) }
    }

    pub fn reserve_registers(&mut self, count: usize) {
        self.registers.reserve(count)
    }

    pub fn emit_raw(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn nop(&mut self) {
        self.emit_raw(Instruction::Nop);
    }

    pub fn const_i<C: Into<instruction_set::IntegerConstant>>(&mut self, constant: C) -> &Register {
        let value = constant.into();
        let value_type = value.value_type();
        self.emit_raw(Instruction::ConstI(value));
        self.allocate_register(format::type_system::Any::Primitive(
            format::type_system::Primitive::from(value_type),
        ))
    }

    //pub fn ret<R: std::iter::IntoIterator<Item = &Register>>(&mut self: registers: R)
}
