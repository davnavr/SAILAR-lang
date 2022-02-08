use crate::format::instruction_set;
use crate::{builder, format};
use std::rc::Rc;

pub use instruction_set::{BasicArithmeticOperation, Instruction, OverflowBehavior};

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned, but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
    #[error("expected {expected} arguments to be provided to function, but got {actual}")]
    ArgumentCountMismatch {
        function: builder::Function,
        expected: u32,
        actual: u32,
    },
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

#[non_exhaustive]
pub struct Block {
    index: format::indices::CodeBlock,
    type_signatures: Rc<builder::TypeSignatures>,
    input_count: u32,
    input_registers: Box<[Register]>,
    return_count: u32,
    instructions: std::cell::RefCell<Vec<Instruction>>,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    registers: typed_arena::Arena<Register>,
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("index", &self.index)
            .field("input_count", &self.input_count)
            .field("return_count", &self.return_count)
            .field("instructions", &self.instructions)
            .finish_non_exhaustive()
    }
}

impl Block {
    pub(super) fn new(
        index: format::indices::CodeBlock,
        type_signatures: Rc<builder::TypeSignatures>,
        mut input_types: Vec<Rc<builder::Type>>,
        return_count: u32,
    ) -> Self {
        Self {
            index,
            type_signatures,
            input_count: input_types.len().try_into().unwrap(),
            input_registers: input_types
                .drain(..)
                .zip(0u32..)
                .map(|(input_type, index)| Register {
                    value_type: input_type,
                    index: format::indices::Register::Input(format::indices::InputRegister::from(
                        index,
                    )),
                })
                .collect(),
            return_count,
            instructions: std::cell::RefCell::new(Vec::new()),
            register_index: builder::counter::Cell::new(),
            registers: typed_arena::Arena::new(),
        }
    }

    pub fn index(&self) -> format::indices::CodeBlock {
        self.index
    }

    pub fn input_count(&self) -> u32 {
        self.input_count
    }

    pub fn input_registers(&self) -> &[Register] {
        &self.input_registers
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

    pub fn call<'a, A>(&'a self, callee: &builder::Function, arguments: A) -> Result<Vec<&Register>>
    where
        A: std::iter::IntoIterator<Item = &'a Register>,
    {
        let signature = callee.signature();
        let expected_argument_count = signature.parameter_types().len();
        let mut argument_indices = Vec::with_capacity(expected_argument_count);

        for (argument_register, expected_argument_type) in
            arguments.into_iter().zip(signature.parameter_types())
        {
            if std::borrow::Borrow::<builder::Type>::borrow(expected_argument_type)
                != argument_register.value_type()
            {
                return Err(Error::RegisterTypeMismatch {
                    register: argument_register.index,
                    expected: expected_argument_type.clone(),
                    actual: argument_register.value_type.clone(),
                });
            }

            argument_indices.push(argument_register.index);
        }

        if argument_indices.len() != expected_argument_count {
            return Err(Error::ArgumentCountMismatch {
                function: callee.clone(),
                expected: u32::try_from(expected_argument_count).unwrap(),
                actual: u32::try_from(argument_indices.len()).unwrap(),
            });
        }

        self.emit_raw(Instruction::Call(
            format::instruction_set::CallInstruction {
                function: callee.index(),
                arguments: format::LenVec(argument_indices),
            },
        ));

        Ok(signature
            .result_types()
            .iter()
            .map(|result_type| self.allocate_register(result_type.clone()))
            .collect())
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

    fn allocate_overflow_flag(&self) -> &Register {
        self.allocate_register(
            self.type_signatures
                .primitive(format::type_system::FixedInt::U8),
        )
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
            overflowed: self.allocate_overflow_flag(),
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
        self.allocate_register(self.type_signatures.primitive(value_type))
    }

    fn integer_conversion_operation(
        &self,
        operand: &Register,
        target_type: format::type_system::Int,
        overflow: OverflowBehavior,
    ) -> &Register {
        // TODO: Check that operand is an integer or pointer
        self.emit_raw(Instruction::ConvI {
            target_type,
            overflow,
            operand: operand.index(),
        });
        self.allocate_register(self.type_signatures.primitive(target_type))
    }

    pub fn conv_i(
        &self,
        operand: &Register,
        target_type: format::type_system::Int,
    ) -> Result<&Register> {
        Ok(self.integer_conversion_operation(operand, target_type, OverflowBehavior::Ignore))
    }

    pub fn conv_i_overflowing(
        &self,
        operand: &Register,
        target_type: format::type_system::Int,
    ) -> Result<OverflowingArithmeticResult> {
        Ok(OverflowingArithmeticResult {
            result: self.integer_conversion_operation(operand, target_type, OverflowBehavior::Flag),
            overflowed: self.allocate_overflow_flag(),
        })
    }

    /// Emits a `mem.init` instruction, using module data as a source.
    pub fn mem_init_from_data(&self, destination: &Register, source: Rc<builder::Data>) {
        // TODO: Check that destianation of mem.init is a pointer.
        self.emit_raw(Instruction::MemInit {
            destination: destination.index(),
            source: instruction_set::MemoryInitializationSource::FromData(source.index()),
        });
    }

    pub fn alloca(&self, amount: &Register, element_type: Rc<builder::Type>) -> &Register {
        match amount.value_type().as_raw() {
            format::TypeSignature::Primitive(_) => {
                self.emit_raw(Instruction::Alloca {
                    amount: amount.index(),
                    element_type: element_type.index(),
                });
                self.allocate_register(self.type_signatures.native_pointer(element_type))
            }
            _ => todo!("amount register should be an integer type"),
        }
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
#[derive(Debug)]
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
