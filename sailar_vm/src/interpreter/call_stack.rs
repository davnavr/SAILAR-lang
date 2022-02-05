use crate::interpreter::{self, debugger, ErrorKind, Result};
use interpreter::{BlockIndex, InstructionLocation, Register};
use sailar::format::{self, indices, instruction_set};
use std::borrow::{Borrow as _, BorrowMut as _};
use std::collections::{hash_map, BTreeSet, VecDeque};

/// Describes a stack frame in the call stack.
#[derive(Clone, Debug)]
pub struct TraceFrame {
    depth: usize,
    location: Option<InstructionLocation>,
    function: debugger::FunctionSymbol<'static>,
    input_registers: Box<[Register]>,
    temporary_registers: Box<[Register]>,
}

impl TraceFrame {
    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn location(&self) -> &Option<InstructionLocation> {
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
    temporaries: Vec<Register>,
}

impl Registers {
    pub fn inputs(&self) -> &[Register] {
        &self.inputs
    }

    pub fn define_temporary(&mut self, temporary: Register) {
        self.temporaries.push(temporary)
    }

    pub fn append_temporaries(&mut self, temporaries: &mut Vec<Register>) {
        self.temporaries.append(temporaries)
    }

    /// Retrieves the register associated with the specified index.
    pub fn get(&self, index: indices::Register) -> Result<&Register> {
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
            indices::Register::Temporary(temporary_index) => {
                lookup_register!(temporary_index, self.temporaries)
            }
            indices::Register::Input(input_index) => lookup_register!(input_index, self.inputs),
        }
    }
}

// pub enum Code<'l> {
//     Defined(&'l format::Code),
//     //External(&'l )
// }

pub(super) struct InstructionPointer<'l> {
    code: &'l format::Code,
    block_index: BlockIndex,
    previous_block: Option<BlockIndex>,
    instructions: &'l [instruction_set::Instruction],
    last_breakpoint_hit: Option<usize>,
    /// Instruction offsets marking where breakpoints are placed, in increasing order.
    breakpoints: VecDeque<usize>,
}

static BREAK: &instruction_set::Instruction = &instruction_set::Instruction::Break;

impl<'l> InstructionPointer<'l> {
    fn new(code: &'l format::Code) -> Self {
        Self {
            code,
            block_index: BlockIndex::entry(),
            previous_block: None,
            instructions: &code.entry_block.instructions,
            breakpoints: VecDeque::new(),
            last_breakpoint_hit: None,
        }
    }

    pub fn current_block(&self) -> &'l format::CodeBlock {
        match self.block_index {
            BlockIndex(None) => &self.code.entry_block,
            BlockIndex(Some(other_index)) => &self.code.blocks[other_index],
        }
    }

    pub fn code_index(&self) -> usize {
        unsafe {
            self.instructions
                .as_ptr()
                .offset_from(self.current_block().instructions.0.as_ptr()) as usize
        }
    }

    fn location(&self) -> InstructionLocation {
        InstructionLocation {
            block_index: self.block_index,
            code_index: self.code_index(),
        }
    }

    fn jump(&mut self, target: BlockIndex) -> Result<()> {
        match target
            .0
            .map(|index| self.code.blocks.get(index))
            .unwrap_or(Some(&self.code.entry_block))
        {
            Some(block) => {
                self.previous_block = Some(self.block_index);
                self.block_index = target;
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
                    self.last_breakpoint_hit = self.breakpoints.pop_front();
                }
                next == current_index
            }
            None => false,
        }
    }

    pub fn next_instruction(&mut self) -> Option<&'l instruction_set::Instruction> {
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

pub(super) enum Code<'l> {
    Defined(InstructionPointer<'l>),
    External(&'l dyn interpreter::ffi::Handler),
}

pub struct Frame<'l> {
    depth: usize,
    previous: Option<Box<Frame<'l>>>,
    function: interpreter::LoadedFunction<'l>,
    pub(super) result_count: usize,
    pub(super) registers: Registers,
    code: Code<'l>,
}

impl<'l> Frame<'l> {
    pub fn function(&self) -> interpreter::LoadedFunction<'l> {
        self.function
    }

    pub fn trace(&self) -> TraceFrame {
        TraceFrame {
            depth: self.depth,
            location: match &self.code {
                Code::Defined(instructions) => Some(instructions.location()),
                Code::External(_) => None,
            },
            function: self.function.full_symbol().unwrap().to_owned(),
            input_registers: self.registers.inputs.clone().into_boxed_slice(),
            temporary_registers: self.registers.temporaries.clone().into_boxed_slice(),
        }
    }

    pub(super) fn code_mut(&mut self) -> &mut Code<'l> {
        &mut self.code
    }

    fn instructions(&mut self) -> Result<&mut InstructionPointer<'l>> {
        match &mut self.code {
            Code::Defined(ref mut instructions) => Ok(instructions),
            _ => Err(ErrorKind::UndefinedFunctionBody(self.function.index())),
        }
    }

    /// Updates the instruction pointer to point at the specified target.
    ///
    /// The breakpoint list must be updated afterward.
    fn jump(&mut self, target: BlockIndex, inputs: &[Register]) -> Result<()> {
        self.registers.temporaries.clear();

        // Replace input registers with new inputs.
        self.registers.inputs.clear();
        self.registers.inputs.extend_from_slice(inputs);

        self.instructions()?.jump(target)?;

        let expected_input_count = self
            .instructions()?
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

        Ok(())
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Breakpoint {
    location: InstructionLocation,
    function: debugger::FunctionSymbol<'static>,
}

impl Breakpoint {
    pub fn with_location(
        location: InstructionLocation,
        function: debugger::FunctionSymbol<'static>,
    ) -> Self {
        Self { location, function }
    }

    pub fn with_symbol(
        block: BlockIndex,
        instruction: usize,
        function: debugger::FunctionSymbol<'static>,
    ) -> Self {
        Self::with_location(
            InstructionLocation {
                block_index: block,
                code_index: instruction,
            },
            function,
        )
    }

    pub fn new(
        block: BlockIndex,
        instruction: usize,
        module: debugger::ModuleSymbol<'static>,
        symbol: debugger::Symbol<'static>,
    ) -> Self {
        Self::with_symbol(
            block,
            instruction,
            debugger::FunctionSymbol::new(module, symbol),
        )
    }

    pub fn new_owned(
        block: BlockIndex,
        instruction: usize,
        module: debugger::ModuleIdentifier,
        symbol: sailar::format::Identifier,
    ) -> Self {
        Self::new(
            block,
            instruction,
            debugger::ModuleSymbol::Owned(module),
            debugger::Symbol::Owned(symbol),
        )
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
    pub fn is_empty(&self) -> bool {
        self.lookup.is_empty()
    }

    pub(super) fn clear(&mut self) {
        self.lookup.clear()
    }

    fn copy_breakpoints_to(
        &mut self,
        function: &debugger::FunctionSymbol<'static>,
        block: BlockIndex,
        current_instruction: usize,
        last_breakpoint_hit: Option<usize>,
        destination: &mut VecDeque<usize>,
    ) {
        if !self.is_empty() {
            destination.clear();

            if let Some(indices) = self
                .lookup
                .get(function)
                .and_then(|block_lookup| block_lookup.get(&block))
            {
                if destination.capacity() < indices.len() {
                    destination.reserve_exact(indices.len() - destination.capacity());
                }

                for &index in indices {
                    if index >= current_instruction && Some(index) != last_breakpoint_hit {
                        destination.push_back(index);
                    }
                }
            }
        }
    }

    fn update_in<'l>(&mut self, frame: &mut Frame<'l>) -> Result<()> {
        let function = &frame.function.full_symbol()?.to_owned();
        let instructions = frame.instructions()?;

        self.copy_breakpoints_to(
            function,
            instructions.block_index,
            instructions.code_index(),
            instructions.last_breakpoint_hit,
            &mut instructions.breakpoints,
        );

        Ok(())
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
                block_lookup
                    .iter()
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

pub use std::num::NonZeroUsize as Capacity;

pub struct Stack<'l> {
    current: Option<Box<Frame<'l>>>,
    capacity: Capacity,
    breakpoints: BreakpointLookup,
}

impl<'l> Stack<'l> {
    pub(super) fn new(capacity: Capacity) -> Self {
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

    pub(super) fn peek(&self) -> Option<&Frame<'l>> {
        self.current.as_ref().map(Box::borrow)
    }

    pub(super) fn current(&self) -> Result<&Frame<'l>> {
        self.peek().ok_or(ErrorKind::CallStackUnderflow)
    }

    pub(super) fn peek_mut(&mut self) -> Option<&mut Frame<'l>> {
        self.current.as_mut().map(Box::borrow_mut)
    }

    pub(super) fn current_mut(&mut self) -> Result<&mut Frame<'l>> {
        self.peek_mut().ok_or(ErrorKind::CallStackUnderflow)
    }

    pub(super) fn pop(&mut self) -> Result<Box<Frame<'l>>> {
        match self.current.take() {
            Some(mut current) => {
                self.current = current.previous.take();
                if let Some(new_current) = &mut self.current {
                    self.breakpoints.update_in(new_current)?;
                }
                Ok(current)
            }
            None => Err(ErrorKind::CallStackUnderflow),
        }
    }

    pub(super) fn push(
        &mut self,
        function: interpreter::LoadedFunction<'l>,
        arguments: &[Register],
        external_call_handler: &'l interpreter::ffi::HandlerLookup,
    ) -> Result<()> {
        let depth = self.depth() + 1;
        if depth > self.capacity.get() {
            return Err(ErrorKind::CallStackOverflow(self.capacity));
        }

        let signature = function.signature()?;

        if signature.parameter_types().len() != arguments.len() {
            return Err(ErrorKind::InputCountMismatch {
                expected: signature.parameter_types().len(),
                actual: arguments.len(),
            });
        }

        let inputs = arguments.to_vec();

        match function.raw_body() {
            format::FunctionBody::Defined(code_index) => {
                let code = function.declaring_module().load_code_source(*code_index)?;
                let previous = self.current.take();
                let mut frame = Box::new(Frame {
                    depth,
                    function,
                    previous,
                    result_count: signature.return_types().len(),
                    registers: Registers {
                        inputs,
                        temporaries: Vec::new(),
                    },
                    code: Code::Defined(InstructionPointer::new(code)),
                });

                self.breakpoints.update_in(&mut frame)?;
                self.current = Some(frame);
            }
            format::FunctionBody::External {
                library,
                entry_point_name,
            } => {
                let current_module = function.declaring_module();
                let library_name = current_module.load_identifier_raw(*library)?;
                let entry_point_symbol = current_module.load_identifier_raw(*entry_point_name)?;
                let previous = self.current.take();

                self.current = Some(Box::new(Frame {
                    depth,
                    function,
                    previous,
                    result_count: signature.return_types().len(),
                    registers: Registers {
                        inputs,
                        temporaries: Vec::new(),
                    },
                    code: Code::External(
                        external_call_handler
                            .get(library_name, entry_point_symbol)
                            .and_then(|handler| {
                                handler.ok_or_else(|| {
                                    todo!(
                                        "actual external calls are not yet supported (libloading)"
                                    )
                                })
                            })?,
                    ),
                }));
            }
        }

        Ok(())
    }

    pub(super) fn update_current_breakpoints(&mut self) -> Result<()> {
        if let Some(mut current) = self.current.take() {
            self.breakpoints.update_in(&mut current)?;
            self.current = Some(current);
        }
        Ok(())
    }

    pub(super) fn current_jump_to(
        &mut self,
        target: BlockIndex,
        inputs: &[Register],
    ) -> Result<()> {
        // Duplicate code with update_current_breakpoints
        if let Some(mut current) = self.current.take() {
            current.jump(target, inputs)?;
            self.breakpoints.update_in(&mut current)?;
            self.current = Some(current);
        }
        Ok(())
    }

    // TODO: Could have a function that returns a struct that temporarily takes ownership of Current frame then puts it back on top of call stack.
    //pub(super) fn something_current_frame(&mut self) -> CurrentFrame<'l>
    // or
    //pub(super) fn something_else_current_frame<F: FnOnce(&mut Frame<'l>)>(&mut self, f: F) -> Result<()>
}
