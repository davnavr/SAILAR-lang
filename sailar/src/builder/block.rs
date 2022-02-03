use crate::format::instruction_set::{self, Instruction};
use crate::{builder, format};

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Block {
    index: format::indices::CodeBlock,
    type_signatures: std::rc::Rc<builder::TypeSignatures>,
    input_count: u32,
    return_count: u32,
    instructions: std::cell::RefCell<Vec<Instruction>>,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register>,
}

impl Block {
    pub(super) fn new(
        index: format::indices::CodeBlock,
        type_signatures: std::rc::Rc<builder::TypeSignatures>,
        input_count: u32,
        return_count: u32,
    ) -> Self {
        Self {
            index,
            type_signatures,
            input_count,
            return_count,
            instructions: std::cell::RefCell::new(Vec::new()),
            register_index: builder::counter::Cell::new(),
            registers: typed_arena::Arena::new(),
        }
    }

    pub fn index(&self) -> format::indices::CodeBlock {
        self.index
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&self) -> u32 {
        self.return_count
    }

    fn allocate_register(&self, value_type: std::rc::Rc<builder::Type>) -> &Register {
        self.registers.alloc(Register {
            index: format::indices::Register::Temporary(self.register_index.next()),
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

    pub fn ret<'a, R>(&'a self, results: R) -> Result<()>
    where
        R: std::iter::IntoIterator<Item = &'a Register>,
    {
        let result_indices = results
            .into_iter()
            .map(|register| register.index)
            .collect::<Vec<_>>();

        let actual_count = result_indices.len().try_into().unwrap();

        self.emit_raw(Instruction::Ret(format::LenVec(result_indices)));

        if actual_count == self.return_count {
            Ok(())
        } else {
            Err(Error::ResultCountMismatch {
                expected: self.return_count,
                actual: actual_count,
            })
        }
    }

    pub fn const_i<C>(&self, constant: C) -> &Register
    where
        C: Into<instruction_set::IntegerConstant>,
    {
        let value = constant.into();
        let value_type = value.value_type();
        self.emit_raw(Instruction::ConstI(value));
        self.allocate_register(self.type_signatures.primitive_type(value_type))
    }

    pub(super) fn build(&mut self) -> format::CodeBlock {
        format::CodeBlock {
            input_register_count: format::numeric::UInteger(self.input_count),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(std::cell::RefCell::take(
                &self.instructions,
            ))),
        }
    }
}

// Borrowing rules should ensure registers from other blocks cannot be used if only one builder::Block can exist at a time. Maybe have separate type for jump target (JumpTarget)?
// TODO: How to ensure registers have correct owners?
pub struct Register {
    index: format::indices::Register,
}

impl Register {
    pub fn index(&self) -> format::indices::Register {
        self.index
    }

    // pub fn value_type(&self) -> &builder::Type<'a> {
    //     &self.value_type
    // }
}
