use crate::format::instruction_set;
use crate::{builder, format};
use std::rc::Rc;

pub use instruction_set::{BasicArithmeticOperation, Instruction, OverflowBehavior};

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
    #[error(
        "expected register {register} to have type {expected:?} but actual type was {actual:?}"
    )]
    RegisterTypeMismatch {
        register: format::indices::Register,
        expected: Rc<builder::Type>,
        actual: Rc<builder::Type>,
    },
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct OverflowingArithmeticResult<'a> {
    result: &'a Register,
    overflowed: &'a Register,
}

impl OverflowingArithmeticResult<'_> {
    pub fn overflowed(&self) -> &Register {
        self.overflowed
    }
    pub fn result(&self) -> &Register {
        self.result
    }
}

pub struct Block {
    index: format::indices::CodeBlock,
    type_signatures: Rc<builder::TypeSignatures>,
    input_count: u32,
    return_count: u32,
    instructions: std::cell::RefCell<Vec<Instruction>>,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register>,
}

impl Block {
    pub(super) fn new(
        index: format::indices::CodeBlock,
        type_signatures: Rc<builder::TypeSignatures>,
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

    fn allocate_register(&self, value_type: Rc<builder::Type>) -> &Register {
        self.registers.alloc(Register {
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

    /// Emits a basic arithmetic instruction.
    ///
    /// The caller should allocate the `flag` register if an overflow is not ignored.
    fn basic_arithmetic_operation<I>(
        &self,
        instruction: I,
        x: &Register,
        y: &Register,
        overflow: OverflowBehavior,
    ) -> Result<&Register>
    where
        I: FnOnce(BasicArithmeticOperation) -> Instruction,
    {
        if x.value_type() != y.value_type() {
            return Err(Error::RegisterTypeMismatch {
                register: y.index,
                expected: x.value_type.clone(),
                actual: y.value_type.clone(),
            });
        }

        // TODO: Ensure that only valid types are allowed for math (e.g. primitives and pointers)
        // TODO: Ensure pointer arithmetic only has a pointer on one side.

        self.emit_raw(instruction(BasicArithmeticOperation {
            overflow,
            x: x.index,
            y: y.index,
        }));

        Ok(self.allocate_register(x.value_type.clone()))
    }

    fn overflowing_arithmetic_operation<I>(
        &self,
        instruction: I,
        x: &Register,
        y: &Register,
    ) -> Result<OverflowingArithmeticResult>
    where
        I: FnOnce(BasicArithmeticOperation) -> Instruction,
    {
        let result = self.basic_arithmetic_operation(instruction, x, y, OverflowBehavior::Flag)?;
        Ok(OverflowingArithmeticResult {
            result,
            overflowed: self.allocate_register(
                self.type_signatures
                    .primitive_type(format::type_system::FixedInt::U8),
            ),
        })
    }

    /// Emits an `add` operation, ignoring any overflows
    pub fn add_overflowing(&self, x: &Register, y: &Register) -> Result<&Register> {
        self.basic_arithmetic_operation(Instruction::Add, x, y, OverflowBehavior::Ignore)
    }

    /// Emits an `add` operation, with a register indicating if an overflow occured.
    pub fn add(&self, x: &Register, y: &Register) -> Result<OverflowingArithmeticResult> {
        self.overflowing_arithmetic_operation(Instruction::Add, x, y)
    }

    /// Emits an `sub` operation, ignoring any overflows
    pub fn sub_overflowing(&self, x: &Register, y: &Register) -> Result<&Register> {
        self.basic_arithmetic_operation(Instruction::Sub, x, y, OverflowBehavior::Ignore)
    }

    /// Emits an `sub` operation, with a register indicating if an overflow occured.
    pub fn sub(&self, x: &Register, y: &Register) -> Result<OverflowingArithmeticResult> {
        self.overflowing_arithmetic_operation(Instruction::Sub, x, y)
    }

    /// Emits an `mul` operation, ignoring any overflows
    pub fn mul_overflowing(&self, x: &Register, y: &Register) -> Result<&Register> {
        self.basic_arithmetic_operation(Instruction::Mul, x, y, OverflowBehavior::Ignore)
    }

    /// Emits an `mul` operation, with a register indicating if an overflow occured.
    pub fn mul(&self, x: &Register, y: &Register) -> Result<OverflowingArithmeticResult> {
        self.overflowing_arithmetic_operation(Instruction::Mul, x, y)
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

    pub(super) fn build(&self) -> format::CodeBlock {
        format::CodeBlock {
            input_register_count: format::numeric::UInteger(self.input_count),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(std::cell::RefCell::take(
                &self.instructions,
            ))),
        }
    }
}

// TODO: How to ensure registers have correct owners?
pub struct Register {
    index: format::indices::Register,
    value_type: Rc<builder::Type>,
}

impl Register {
    pub fn index(&self) -> format::indices::Register {
        self.index
    }

    pub fn value_type(&self) -> &builder::Type {
        &self.value_type
    }
}
