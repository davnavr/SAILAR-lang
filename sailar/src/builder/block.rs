use crate::format::instruction_set;
use crate::{builder, format};
use std::borrow::Borrow as _;
use std::cell::RefCell;
use std::rc::Rc;

pub use instruction_set::{BasicArithmeticOperation, Instruction, OverflowBehavior};

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("expected {expected} values to be returned but got {actual}")]
    ResultCountMismatch { expected: u32, actual: u32 },
    #[error("block {block} expected {expected} inputs but got {actual}")]
    InputCountMismatch {
        block: format::indices::CodeBlock,
        expected: u32,
        actual: u32,
    },
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
    input_types: Box<[Rc<builder::Type>]>,
    return_types: Box<[Rc<builder::Type>]>,
    instructions: RefCell<Vec<Instruction>>,
    register_index: builder::counter::Cell<format::indices::TemporaryRegister>,
    #[allow(clippy::vec_box)]
    registers: RefCell<Vec<Box<Register>>>,
}

impl std::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("index", &self.index)
            .field("input_registers", &self.input_registers)
            .field("return_types", &self.return_types)
            .field("temporary_registers", &self.registers.borrow())
            .field("instructions", &self.instructions)
            .finish_non_exhaustive()
    }
}

impl Block {
    pub(super) fn new(
        index: format::indices::CodeBlock,
        type_signatures: Rc<builder::TypeSignatures>,
        input_types: &[Rc<builder::Type>],
        result_types: &[Rc<builder::Type>],
    ) -> Self {
        let (input_count, input_registers, input_types) = {
            let mut count = 0u32;
            let mut registers = Vec::with_capacity(input_types.len());
            let mut types = Vec::with_capacity(registers.capacity());
            for input_type in input_types.iter() {
                types.push(input_type.clone());

                registers.push(Register {
                    value_type: input_type.clone(),
                    index: format::indices::Register::Input(format::indices::InputRegister::from(
                        count,
                    )),
                });

                count += 1;
            }
            (
                count,
                registers.into_boxed_slice(),
                types.into_boxed_slice(),
            )
        };

        Self {
            index,
            type_signatures,
            input_count,
            input_registers,
            input_types,
            return_types: result_types.to_vec().into_boxed_slice(),
            instructions: RefCell::default(),
            register_index: builder::counter::Cell::new(),
            registers: RefCell::default(),
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

    pub fn input_types(&self) -> &[Rc<builder::Type>] {
        &self.input_types
    }

    /// Returns the number of values that should be returned by any `ret` instruction in this block.
    pub fn expected_return_count(&self) -> u32 {
        u32::try_from(self.return_types.len()).unwrap()
    }

    pub fn expected_return_types(&self) -> &[Rc<builder::Type>] {
        &self.return_types
    }

    #[allow(clippy::needless_lifetimes)]
    fn allocate_register<'a>(&'a self, value_type: Rc<builder::Type>) -> &'a Register {
        let register = Box::new(Register {
            index: format::indices::Register::Temporary(self.register_index.next()),
            value_type,
        });

        let allocated = &*register as *const Register;

        self.registers.borrow_mut().push(register);

        unsafe { &*allocated }
    }

    pub fn reserve_registers(&self, count: usize) {
        self.registers.borrow_mut().reserve(count)
    }

    pub fn emit_raw(&self, instruction: Instruction) {
        self.instructions.borrow_mut().push(instruction);
    }

    pub fn nop(&self) {
        self.emit_raw(Instruction::Nop);
    }

    fn check_register_types<'a, R, E>(
        &'a self,
        registers: R,
        expected_types: &[Rc<builder::Type>],
        count_mismatch: E,
    ) -> Result<Vec<format::indices::Register>>
    where
        R: std::iter::IntoIterator<Item = &'a Register>,
        E: FnOnce(usize) -> Error,
    {
        let all_registers = registers.into_iter();
        let mut indices = Vec::with_capacity({
            let size = all_registers.size_hint();
            size.1.unwrap_or(size.0)
        });

        for (index, register) in all_registers.enumerate() {
            match expected_types.get(index) {
                Some(expected_value_type)
                    if register.value_type() == Rc::borrow(expected_value_type) =>
                {
                    indices.push(register.index())
                }
                Some(expected_value_type) => {
                    return Err(Error::RegisterTypeMismatch {
                        register: register.index(),
                        expected: expected_value_type.clone(),
                        actual: register.value_type.clone(),
                    })
                }
                None => return Err(count_mismatch(indices.len())),
            }
        }

        Ok(indices)
    }

    pub fn ret<'a, R>(&'a self, results: R) -> Result<()>
    where
        R: std::iter::IntoIterator<Item = &'a Register>,
    {
        let result_indices =
            self.check_register_types(results, self.expected_return_types(), |actual_count| {
                Error::ResultCountMismatch {
                    expected: self.expected_return_count(),
                    actual: u32::try_from(actual_count).unwrap(),
                }
            })?;

        self.emit_raw(Instruction::Ret(format::LenVec(result_indices)));
        Ok(())
    }

    fn check_target_branch<'a, I>(
        &'a self,
        target: &Self,
        inputs: I,
    ) -> Result<(format::indices::CodeBlock, Vec<format::indices::Register>)>
    where
        I: std::iter::IntoIterator<Item = &'a Register>,
    {
        // TODO: how to get Rc to owning code to check that owners are correct
        let input_indices =
            self.check_register_types(inputs, target.input_types(), |actual_count| {
                Error::InputCountMismatch {
                    block: target.index(),
                    expected: u32::try_from(target.input_types().len()).unwrap(),
                    actual: u32::try_from(actual_count).unwrap(),
                }
            })?;

        Ok((target.index(), input_indices))
    }

    pub fn branch<'a, I>(&'a self, target: &Self, inputs: I) -> Result<()>
    where
        I: std::iter::IntoIterator<Item = &'a Register>,
    {
        let (target_branch, input_indices) = self.check_target_branch(target, inputs)?;

        self.emit_raw(Instruction::Br {
            target: target_branch,
            input_registers: format::LenVec(input_indices),
        });

        Ok(())
    }

    pub fn branch_if<'a, T, F>(
        &'a self,
        condition: &'a Register,
        true_target: &Self,
        true_inputs: T,
        false_target: &Self,
        false_inputs: F,
    ) -> Result<()>
    where
        T: std::iter::IntoIterator<Item = &'a Register>,
        F: std::iter::IntoIterator<Item = &'a Register>,
    {
        let (true_target_branch, true_input_indices) =
            self.check_target_branch(true_target, true_inputs)?;
        let (false_target_branch, false_input_indices) =
            self.check_target_branch(false_target, false_inputs)?;

        self.emit_raw(Instruction::BrIf {
            condition: condition.index(),
            true_branch: true_target_branch,
            true_inputs: format::LenVec(true_input_indices),
            false_branch: false_target_branch,
            false_inputs: format::LenVec(false_input_indices),
        });

        Ok(())
    }

    pub fn call<'a, A>(&'a self, callee: &builder::Function, arguments: A) -> Result<Vec<&Register>>
    where
        A: std::iter::IntoIterator<Item = &'a Register>,
    {
        let signature = callee.signature();
        let argument_indices =
            self.check_register_types(arguments, signature.parameter_types(), |actual_count| {
                Error::ArgumentCountMismatch {
                    function: callee.clone(),
                    expected: u32::try_from(signature.parameter_types().len()).unwrap(),
                    actual: u32::try_from(actual_count).unwrap(),
                }
            })?;

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

    pub fn conv_i_overflowing(
        &self,
        operand: &Register,
        target_type: format::type_system::Int,
    ) -> Result<&Register> {
        Ok(self.integer_conversion_operation(operand, target_type, OverflowBehavior::Ignore))
    }

    pub fn conv_i(
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
            input_registers: format::LenVec(
                self.input_registers
                    .iter()
                    .map(|input| input.value_type.index())
                    .collect(),
            ),
            temporary_registers: format::LenVec(
                self.registers
                    .borrow()
                    .iter()
                    .map(|register| register.value_type.index())
                    .collect(),
            ),
            exception_handler: None,
            instructions: format::LenBytes(format::LenVec(RefCell::take(&self.instructions))),
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
