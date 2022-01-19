use super::*;
use std::{
    borrow::{Borrow as _, BorrowMut as _},
    collections::{hash_map, BTreeSet, VecDeque},
};

/// Describes a stack frame in the call stack.
#[derive(Clone, Debug)]
pub struct TraceFrame {
    // TODO: fields here should be private.
    depth: usize,
    location: InstructionLocation,
    function: debugger::FunctionSymbol<'static>,
    input_registers: Box<[Register]>,
    temporary_registers: Box<[Register]>,
}

impl TraceFrame {
    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    pub fn function(&self) -> &debugger::FunctionSymbol<'static> {
        &self.function
    }

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
    /// Instruction offsets marking where breakpoints are placed, in increasing order.
    breakpoints: VecDeque<usize>,
}

static BREAK: &'static Instruction = &Instruction::Break;

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

    fn check_breakpoint_hit(&mut self) -> bool {
        match self.breakpoints.front() {
            Some(&next) => {
                let current_index = self.code_index();
                if next <= current_index {
                    self.breakpoints.pop_front();
                }
                next == current_index
            }
            None => false,
        }
    }

    pub fn next_instruction(&mut self) -> Option<&'l Instruction> {
        if self.check_breakpoint_hit()
        /* || self.move_next */
        {
            Some(BREAK)
        } else {
            let next = self.instructions.first();
            if next.is_some() {
                self.instructions = &self.instructions[1..];
            }
            next
        }
    }
}

pub struct Frame<'l> {
    depth: usize,
    previous: Option<Box<Frame<'l>>>,
    pub(super) registers: Registers,
    pub(super) instructions: InstructionPointer<'l>,
}

impl<'l> Frame<'l> {
    pub fn trace(&self) -> TraceFrame {
        todo!()
    }

    pub(super) fn jump(&mut self, target: JumpTarget, inputs: &[Register]) -> Result<()> {
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

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Breakpoint {
    location: InstructionLocation,
    function: debugger::FunctionSymbol<'static>,
}

impl Breakpoint {
    pub fn new(location: InstructionLocation, function: debugger::FunctionSymbol<'static>) -> Self {
        Self { location, function }
    }

    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    pub fn function(&self) -> &debugger::FunctionSymbol<'static> {
        &self.function
    }
}

#[derive(Default)]
pub struct BreakpointLookup {
    lookup: hash_map::HashMap<
        debugger::FunctionSymbol<'static>,
        hash_map::HashMap<BlockIndex, BTreeSet<usize>>,
    >,
}

impl BreakpointLookup {
    fn copy_breakpoints_to(
        &mut self,
        function: &debugger::FunctionSymbol<'static>,
        block: BlockIndex,
        destination: &mut VecDeque<usize>,
    ) {
        destination.clear();
        if let Some(indices) = self
            .lookup
            .get(&function)
            .and_then(|block_lookup| block_lookup.get(&block))
        {
            if destination.capacity() < indices.len() {
                destination.reserve_exact(indices.len() - destination.capacity());
            }
            for &index in indices {
                destination.push_back(index);
            }
        }
    }

    pub fn insert(&mut self, breakpoint: Breakpoint) {
        let blocks = match self.lookup.entry(breakpoint.function) {
            hash_map::Entry::Occupied(occupied) => occupied.into_mut(),
            hash_map::Entry::Vacant(vacant) => vacant.insert(hash_map::HashMap::new()),
        };

        let points = match blocks.entry(breakpoint.location.block_index) {
            hash_map::Entry::Occupied(occupied) => occupied.into_mut(),
            hash_map::Entry::Vacant(vacant) => vacant.insert(BTreeSet::new()),
        };

        points.insert(breakpoint.location.code_index);
    }

    pub fn iter(&self) -> impl std::iter::Iterator<Item = Breakpoint> + '_ {
        self.lookup
            .iter()
            .map(|(function, block_lookup)| {
                block_lookup.iter()
                    .map(move |(&block_index, indices)| {
                        indices.iter().map(move |&code_index| Breakpoint {
                            location: InstructionLocation {
                                block_index,
                                code_index,
                            },
                            function: function.clone(),
                        })
                    })
                    .flatten()
            })
            .flatten()
    }
}

pub struct Stack<'l> {
    current: Option<Box<Frame<'l>>>,
    capacity: usize,
    breakpoints: BreakpointLookup,
}

impl<'l> Stack<'l> {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            current: None,
            capacity,
            breakpoints: BreakpointLookup::default(),
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

    pub fn breakpoints_mut(&mut self) -> &mut BreakpointLookup {
        &mut self.breakpoints
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
                // TODO: Update breakpoints.
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

                let mut breakpoints = VecDeque::new();
                self.breakpoints_mut().copy_breakpoints_to(
                    &function.full_symbol()?.to_owned(),
                    BlockIndex::entry(),
                    &mut breakpoints,
                );

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
                        breakpoints,
                    },
                }));

                // TODO: update breakpoints.

                //self.set_debugger_breakpoints();
                Ok(())
            }
            format::FunctionBody::External { .. } => todo!("external calls not yet supported"),
        }
    }
}
