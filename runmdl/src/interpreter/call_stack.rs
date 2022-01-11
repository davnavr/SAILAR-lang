use super::*;
use std::borrow::{Borrow as _, BorrowMut as _};

/// Describes a stack frame in the call stack.
#[derive(Clone, Debug)]
pub struct TraceFrame {
    // TODO: fields here should be private.
    pub(crate) depth: usize,
    pub(crate) location: InstructionLocation,
    //pub(crate) method: debugger::FullMethodIdentifier,
    pub(crate) input_registers: Box<[Register]>,
    pub(crate) temporary_registers: Box<[Register]>,
}

impl TraceFrame {
    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    // pub fn method(&self) -> &debugger::FullMethodIdentifier {
    //     &self.method
    // }

    pub fn input_registers(&self) -> &[Register] {
        &self.input_registers
    }

    pub fn temporary_registers(&self) -> &[Register] {
        &self.temporary_registers
    }
}

pub type Trace = Vec<TraceFrame>;

pub(crate) struct Registers {
    inputs: Vec<Register>,
    results: Vec<Register>,
    temporaries: Vec<Register>,
}

impl Registers {
    pub fn define_temporary(&mut self, temporary: Register) {
        self.temporaries.push(temporary)
    }

    pub fn append_temporaries(&mut self, temporaries: &mut Vec<Register>) {
        self.temporaries.append(temporaries)
    }

    /// Retrieves the register associated with the specified index.
    pub fn get(&self, index: RegisterIndex) -> Result<&Register> {
        macro_rules! lookup_register {
            ($register_index: expr, $register_lookup: expr) => {{
                let raw_index = usize::try_from($register_index)
                    .map_err(|_| ErrorKind::UndefinedRegister(index))?;
                $register_lookup
                    .get(raw_index)
                    .ok_or(ErrorKind::UndefinedRegister(index))
            }};
        }

        match index {
            RegisterIndex::Temporary(temporary_index) => {
                lookup_register!(temporary_index, self.temporaries)
            }
            RegisterIndex::Input(input_index) => {
                lookup_register!(input_index, self.inputs)
            }
        }
    }

    pub fn get_many(&self, indices: &[RegisterIndex]) -> Result<Vec<&Register>> {
        let mut registers = Vec::with_capacity(indices.len());
        for index in indices {
            registers.push(self.get(*index)?);
        }
        Ok(registers)
    }

    pub fn temporaries(&self) -> &[Register] {
        &self.temporaries
    }

    pub fn results_mut(&mut self) -> &mut [Register] {
        &mut self.results
    }
}

// pub enum Code<'l> {
//     Defined(&'l format::Code),
//     //External(&'l )
// }

pub(crate) struct InstructionPointer<'l> {
    code: &'l format::Code,
    block_index: BlockIndex,
    instructions: &'l [Instruction],
}

impl<'l> InstructionPointer<'l> {
    pub fn current_block(&self) -> &'l format::CodeBlock {
        match self.block_index {
            BlockIndex(None) => &self.code.entry_block,
            BlockIndex(Some(other_index)) => &self.code.blocks[other_index],
        }
    }

    pub fn code_index(&self) -> usize {
        (unsafe {
            self.instructions
                .as_ptr()
                .offset_from(self.current_block().instructions.0.as_ptr()) as usize
        }) / std::mem::size_of::<Instruction>()
    }

    fn jump(&mut self, target: JumpTarget) -> Result<()> {
        let index = usize::try_from(target).map_err(|_| ErrorKind::UndefinedBlock(target))?;
        match self.code.blocks.get(index) {
            Some(block) => {
                self.block_index = BlockIndex(Some(index));
                self.instructions = &block.instructions;
                Ok(())
            }
            None => Err(ErrorKind::UndefinedBlock(target)),
        }
    }

    pub fn next_instruction(&mut self) -> Option<&'l Instruction> {
        // if self.breakpoint_hit() {
        //     Some(BREAK_INSTRUCTION)
        // } else {
        let next = self.instructions.first();
        if next.is_some() {
            self.instructions = &self.instructions[1..];
        }
        next
        //}
    }
}

pub struct Frame<'l> {
    depth: usize,
    previous: Option<Box<Frame<'l>>>,
    pub(crate) registers: Registers,
    pub(crate) instructions: InstructionPointer<'l>,
}

impl<'l> Frame<'l> {
    pub fn trace(&self) -> TraceFrame {
        todo!()
    }

    pub(crate) fn jump(&mut self, target: JumpTarget, inputs: &[Register]) -> Result<()> {
        // Replace input registers with new inputs.
        self.registers.inputs.clear();
        self.registers.inputs.extend_from_slice(inputs);

        // Index into the method body's other blocks, NONE of the indices refer to the entry block.
        self.instructions.jump(target)?;

        let expected_input_count = self
            .instructions
            .current_block()
            .input_register_count
            .try_into()
            .unwrap();
        if expected_input_count != inputs.len() {
            return Err(ErrorKind::InputCountMismatch {
                expected: expected_input_count,
                actual: inputs.len(),
            });
        }

        // if let Some(debugger) = debugger {
        //     // current_frame.breakpoints.source = debugger
        //     //     .breakpoints_in_block(current_frame.current_method, current_frame.block_index);
        //     current_frame.breakpoints.index = 0;
        // }

        Ok(())
    }
}

pub struct Stack<'l> {
    current: Option<Box<Frame<'l>>>,
    capacity: usize,
}

impl<'l> Stack<'l> {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            current: None,
            capacity,
        }
    }

    pub fn depth(&self) -> usize {
        self.current.as_ref().map(|frame| frame.depth).unwrap_or(0)
    }

    pub fn stack_trace(&self) -> Trace {
        let mut current = self.current.as_ref();
        let mut trace = Vec::new();
        while let Some(frame) = current {
            trace.push(frame.trace());
            current = frame.previous.as_ref();
        }
        trace
    }

    pub fn current(&self) -> Result<&Frame<'l>> {
        self.current
            .as_ref()
            .map(Box::borrow)
            .ok_or(ErrorKind::CallStackUnderflow)
    }

    pub(crate) fn peek_mut(&mut self) -> Option<&mut Frame<'l>> {
        self.current.as_mut().map(Box::borrow_mut)
    }

    pub(crate) fn current_mut(&mut self) -> Result<&mut Frame<'l>> {
        self.peek_mut().ok_or(ErrorKind::CallStackUnderflow)
    }

    pub(crate) fn pop(&mut self) -> Result<Vec<Register>> {
        match self.current.take() {
            Some(mut current) => {
                let results = std::mem::take(&mut current.registers.results);
                self.current = current.previous.take();
                Ok(results)
            }
            None => Err(ErrorKind::CallStackUnderflow),
        }
    }

    pub(crate) fn push(
        &mut self,
        function: LoadedFunction<'l>,
        arguments: &[Register],
    ) -> Result<()> {
        let depth = self.depth() + 1;
        if depth > self.capacity {
            return Err(ErrorKind::CallStackOverflow);
        }

        let signature = function.signature()?;
        let mut argument_registers =
            Register::initialize_many(signature.parameter_types().into_iter().copied());
        let result_registers =
            Register::initialize_many(signature.return_types().into_iter().copied());

        if argument_registers.len() != arguments.len() {
            return Err(ErrorKind::InputCountMismatch {
                expected: argument_registers.len(),
                actual: arguments.len(),
            });
        }

        for (i, register) in argument_registers.iter_mut().enumerate() {
            register.value = arguments[i].value.clone();
        }

        match function.raw_body() {
            format::FunctionBody::Defined(code_index) => {
                let code = function.declaring_module().load_code_raw(*code_index)?;
                let previous = self.current.take();

                self.current = Some(Box::new(Frame {
                    depth: depth,
                    previous,
                    registers: Registers {
                        inputs: argument_registers,
                        results: result_registers,
                        temporaries: Vec::new(),
                    },
                    instructions: InstructionPointer {
                        code,
                        block_index: BlockIndex::entry(),
                        instructions: &code.entry_block.instructions,
                    },
                }));

                //self.set_debugger_breakpoints();
                Ok(())
            }
            format::FunctionBody::External { .. } => todo!("external calls not yet supported"),
        }
    }
}
