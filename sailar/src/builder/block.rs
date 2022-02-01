use crate::format::instruction_set::{self, Instruction};
use crate::format::type_system;
use crate::{builder, format};

pub struct Block {
    index: format::indices::CodeBlock,
    input_count: u32,
    return_count: u32,
    instructions: std::cell::RefCell<Vec<Instruction>>,
}

impl Block {
    pub fn new(index: format::indices::CodeBlock, input_count: u32, return_count: u32) -> Self {
        Self {
            index,
            input_count,
            return_count,
            instructions: std::cell::RefCell::new(Vec::new()),
        }
    }

    pub fn build(&mut self) -> format::CodeBlock {
        format::CodeBlock {
            input_register_count: format::numeric::UInteger(self.input_count),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(std::cell::RefCell::take(
                &self.instructions,
            ))),
        }
    }
}

//     fn allocate_register(&'b self, value_type: &'b builder::Type) -> &'b Register {
//         self.registers.alloc(Register {
//             builder: self.builder,
//             owner: &self.register_owner,
//             index: format::indices::Register::Temporary(self.register_index.next()),
//             value_type,
//         })
//     }

//     pub fn reserve_registers(&'b self, count: usize) {
//         self.registers.reserve_extend(count)
//     }

//     fn check_register(&'b self, register: &'b Register<'b>) -> Result<format::indices::Register> {
//         if register.owner != &self.register_owner {
//             Err(Error::RegisterOwnerMismatch {
//                 register: register.index,
//                 expected: self.register_owner.clone(),
//                 actual: register.owner.clone(),
//             })
//         } else {
//             Ok(register.index)
//         }
//     }

//     fn check_many_registers<R>(&'b self, registers: R) -> Result<Vec<format::indices::Register>>
//     where
//         R: std::iter::IntoIterator<Item = &'b Register<'b>>,
//     {
//         let iterator = registers.into_iter();
//         let mut indices = Vec::with_capacity({
//             let (lower, upper) = iterator.size_hint();
//             upper.unwrap_or(lower)
//         });

//         for register in iterator {
//             indices.push(self.check_register(register)?);
//         }

//         Ok(indices)
//     }

//     pub fn emit_raw(&'b self, instruction: Instruction) {
//         self.instructions.borrow_mut().push(instruction);
//     }

//     pub fn nop(&'b self) {
//         self.emit_raw(Instruction::Nop);
//     }

//     pub fn ret<R>(&'b self, results: R) -> Result<()>
//     where
//         R: std::iter::IntoIterator<Item = &'b Register<'b>>,
//     {
//         let result_indices = self.check_many_registers(results)?;
//         let actual_count = result_indices.len().try_into().unwrap();

//         self.emit_raw(Instruction::Ret(format::LenVec(result_indices)));

//         if actual_count == self.expected_return_count {
//             Ok(())
//         } else {
//             Err(Error::ResultCountMismatch {
//                 expected: self.expected_return_count,
//                 actual: actual_count,
//             })
//         }
//     }

//     pub(super) fn build(&mut self) -> format::CodeBlock {
//         format::CodeBlock {
//             input_register_count: format::numeric::UInteger(self.input_count),
//             exception_handler: None,
//             instructions: format::LenBytes(format::LenVec(self.instructions.take())),
//         }
//     }
// }

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
}

pub type Result<T> = std::result::Result<T, Error>;

// Borrowing rules should ensure registers from other blocks cannot be used if only one builder::Block can exist at a time. Maybe have separate type for jump target (JumpTarget)?
// TODO: How to ensure registers have correct owners?
pub struct Register<'a> {
    index: format::indices::Register,
    value_type: builder::Type<'a>,
}

impl<'a> Register<'a> {
    pub fn index(&self) -> format::indices::Register {
        self.index
    }

    pub fn value_type(&self) -> &builder::Type<'a> {
        &self.value_type
    }
}

pub struct Builder<'a> {
    block: &'a Block,
    owner_index: format::indices::Code,
    builder: &'a (),
    type_signatures: &'a builder::type_signatures::Signatures,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register<'a>>,
}

impl<'a, 'b> Builder<'a>
where
    'a: 'b,
{
    pub(super) fn new(
        block: &'a Block,
        owner_index: format::indices::Code,
        builder: &'a (),
        type_signatures: &'a builder::type_signatures::Signatures,
    ) -> Self {
        Self {
            block,
            owner_index,
            builder,
            type_signatures,
            register_index: builder::counter::Cell::new(),
            registers: typed_arena::Arena::new(),
        }
    }

    pub fn index(&'b self) -> format::indices::CodeBlock {
        self.block.index
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&'b self) -> u32 {
        self.block.return_count
    }

    fn type_signatures(&'b self) -> builder::TypeSignatures<'a> {
        builder::TypeSignatures::<'a> {
            builder: self.builder,
            signatures: self.type_signatures,
        }
    }

    fn allocate_register(&'b self, value_type: builder::Type<'a>) -> &'b Register<'a> {
        self.registers.alloc(Register {
            index: format::indices::Register::Temporary(self.register_index.next()),
            value_type,
        })
    }

    pub fn emit_raw(&'b self, instruction: Instruction) {
        self.block.instructions.borrow_mut().push(instruction);
    }

    pub fn nop(&'b self) {
        self.emit_raw(Instruction::Nop);
    }

    pub fn const_i<C>(&'b self, constant: C) -> &'b Register<'a>
    where
        C: Into<instruction_set::IntegerConstant>,
    {
        let value = constant.into();
        let value_type = value.value_type();
        self.emit_raw(Instruction::ConstI(value));
        let a: builder::Type<'a> = self.type_signatures().primitive(value_type);
        self.allocate_register(a)
    }

    pub fn ret<R>(&'b self, results: R) -> Result<()>
    where
        R: std::iter::IntoIterator<Item = &'b Register<'a>>,
    {
        let result_indices = results
            .into_iter()
            .map(|register| register.index)
            .collect::<Vec<_>>();

        let actual_count = result_indices.len().try_into().unwrap();

        self.emit_raw(Instruction::Ret(format::LenVec(result_indices)));

        if actual_count == self.expected_return_count() {
            Ok(())
        } else {
            Err(Error::ResultCountMismatch {
                expected: self.expected_return_count(),
                actual: actual_count,
            })
        }
    }
}
