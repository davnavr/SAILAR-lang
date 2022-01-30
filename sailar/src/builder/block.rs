use crate::format::instruction_set::{self, Instruction};
use crate::{
    builder::{self, BuilderIdentifier},
    format,
};

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct RegisterOwner {
    pub code_index: format::indices::Code,
    pub block_index: format::indices::CodeBlock,
}

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected register {register} in code {} block {} but got code {} block {}", .expected.code_index, .expected.block_index, .actual.code_index, .actual.block_index)]
    RegisterOwnerMismatch {
        register: format::indices::Register,
        expected: RegisterOwner,
        actual: RegisterOwner,
    },
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Register<'b> {
    builder: BuilderIdentifier<'b>,
    owner: &'b RegisterOwner,
    index: format::indices::Register,
    value_type: &'b builder::Type<'b>,
}

impl<'b> Register<'b> {
    pub fn index(&'b self) -> format::indices::Register {
        self.index
    }

    pub fn value_type(&'b self) -> &'b builder::Type<'b> {
        self.value_type
    }
}

// Uses interior mutability, otherwise problems with borrowing occur if register references are used.
pub struct Block<'b> {
    builder: BuilderIdentifier<'b>,
    register_owner: RegisterOwner,
    input_count: u32,
    expected_return_count: u32,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register<'b>>,
    instructions: std::cell::RefCell<Vec<Instruction>>,
}

impl<'b> Block<'b> {
    pub(super) fn new(
        builder: builder::BuilderIdentifier<'b>,
        owner_index: format::indices::Code,
        index: format::indices::CodeBlock,
        input_count: u32,
        expected_return_count: u32,
    ) -> Self {
        Self {
            builder,
            register_owner: RegisterOwner {
                code_index: owner_index,
                block_index: index,
            },
            input_count,
            expected_return_count,
            register_index: builder::counter::Cell::new(),
            registers: typed_arena::Arena::new(),
            instructions: std::cell::RefCell::new(Vec::new()),
        }
    }

    pub fn index(&'b self) -> format::indices::CodeBlock {
        self.register_owner.block_index
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&'b self) -> u32 {
        self.expected_return_count
    }

    fn allocate_register(&'b self, value_type: &'b builder::Type) -> &'b Register {
        self.registers.alloc(Register {
            builder: self.builder,
            owner: &self.register_owner,
            index: format::indices::Register::Temporary(self.register_index.next()),
            value_type,
        })
    }

    pub fn reserve_registers(&self, count: usize) {
        self.registers.reserve_extend(count)
    }

    fn check_register(&'b self, register: &'b Register<'b>) -> Result<format::indices::Register> {
        if register.owner != &self.register_owner {
            Err(Error::RegisterOwnerMismatch {
                register: register.index,
                expected: self.register_owner.clone(),
                actual: register.owner.clone(),
            })
        } else {
            Ok(register.index)
        }
    }

    fn check_many_registers<R: std::iter::IntoIterator<Item = &'b Register<'b>>>(
        &'b self,
        registers: R,
    ) -> Result<Vec<format::indices::Register>> {
        let iterator = registers.into_iter();
        let mut indices = Vec::with_capacity({
            let (lower, upper) = iterator.size_hint();
            upper.unwrap_or(lower)
        });

        for register in iterator {
            indices.push(self.check_register(register)?);
        }

        Ok(indices)
    }

    pub fn emit_raw(&'b self, instruction: Instruction) {
        self.instructions.borrow_mut().push(instruction);
    }

    pub fn nop(&'b self) {
        self.emit_raw(Instruction::Nop);
    }

    pub fn const_i<C: Into<instruction_set::IntegerConstant>>(
        &'b self,
        constant: C,
    ) -> &'b Register<'b> {
        let value = constant.into();
        let value_type = value.value_type();
        self.emit_raw(Instruction::ConstI(value));
        self.allocate_register(
            todo!("how to access type signatures?"), /* format::type_system::Any::Primitive(
                                                         format::type_system::Primitive::from(value_type),
                                                     ) */
        )
    }

    pub fn ret<R: std::iter::IntoIterator<Item = &'b Register<'b>>>(
        &'b self,
        results: R,
    ) -> Result<()> {
        let result_indices = self.check_many_registers(results)?;
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

    pub(super) fn build(&'b mut self) -> format::CodeBlock {
        format::CodeBlock {
            input_register_count: format::numeric::UInteger(self.input_count),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(self.instructions.take())),
        }
    }
}
