//! Module for interacting with the SAILAR virtual machine's call stack.

use crate::runtime;
use crate::value;
use sailar_load::code_block::Instruction;
use std::cell::Cell;
use std::fmt::{Debug, Formatter};

type CodeBlock = std::sync::Arc<sailar_load::code_block::Code>;

#[derive(Clone, Debug)]
pub struct DefinedFrame {
    arguments: Box<[value::Value]>,
    block: CodeBlock,
    instruction_index: Cell<usize>,
    temporary_index: Cell<usize>,
    temporary_registers: Box<[value::Value]>,
}

impl DefinedFrame {
    fn new(arguments: Box<[value::Value]>, block: CodeBlock) -> runtime::Result<Self> {
        assert_eq!(arguments.len(), block.input_types()?.len());

        Ok(Self {
            arguments,
            instruction_index: Cell::new(0),
            temporary_index: Cell::new(0),
            temporary_registers: vec![value::Value::default(); block.temporary_types()?.len()].into_boxed_slice(),
            block,
        })
    }

    pub fn arguments(&self) -> &[value::Value] {
        &self.arguments
    }

    pub fn instruction_index(&self) -> usize {
        self.instruction_index.get()
    }

    pub fn temporary_registers(&self) -> &[value::Value] {
        &self.temporary_registers[..self.temporary_index.get()]
    }

    pub(crate) fn next_instruction(&self) -> runtime::Result<Option<&Instruction>> {
        let current_index = self.instruction_index.get();
        let instruction = self.block.instructions()?.get(current_index);
        if instruction.is_some() {
            self.instruction_index.set(current_index + 1);
        }
        Ok(instruction)
    }

    /// Defines a new temporary register to contain the specified `value`.
    ///
    /// # Panics
    ///
    /// Panics if the number of defined temporary registers exceeds the number defined in the code block.
    pub(crate) fn define_temporary(&mut self, value: value::Value) {
        let current_index = self.temporary_index.get();

        assert!(current_index < self.temporary_registers.len());

        self.temporary_registers[current_index] = value;
        self.temporary_index.set(current_index + 1);
    }

    /// Gets the value contained in the specified register.
    ///
    /// # Panics
    ///
    /// Panics if a register corresponding to the `index` is not defined.
    pub(crate) fn get_register_value(&self, index: sailar::index::Register) -> &value::Value {
        let i = usize::from(index);
        if let Some(argument) = self.arguments.get(i) {
            argument
        } else {
            self.temporary_registers()
                .get(i - self.arguments.len())
                .expect("register must be defined")
        }
    }

    pub(crate) fn map_typed_value(&self, value: &sailar_load::code_block::TypedValue) -> value::Value {
        match value.raw_value() {
            sailar::instruction::Value::Constant(constant) => value::Value::from_constant(constant.clone()),
            sailar::instruction::Value::IndexedRegister(index) => self.get_register_value(*index).clone(),
        }
    }

    pub(crate) fn map_many_typed_values<'a, V>(&self, values: V) -> Box<[value::Value]>
    where
        V: IntoIterator<Item = &'a sailar_load::code_block::TypedValue>,
        V::IntoIter: std::iter::ExactSizeIterator,
    {
        values.into_iter().map(|value| self.map_typed_value(value)).collect()
    }
}

#[derive(Clone, Debug)]
pub enum FrameKind {
    Defined(DefinedFrame),
    //Foreign(),
}

impl FrameKind {
    pub fn arguments(&self) -> &[value::Value] {
        match self {
            FrameKind::Defined(defined) => defined.arguments(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    function: runtime::Function,
    kind: FrameKind,
}

impl Frame {
    pub fn kind(&self) -> &FrameKind {
        &self.kind
    }

    pub(crate) fn kind_mut(&mut self) -> &mut FrameKind {
        &mut self.kind
    }

    pub fn return_types(&self) -> runtime::Result<&[std::sync::Arc<sailar_load::type_system::Signature>]> {
        Ok(self.function.signature()?.return_types()?)
    }
}

//pub struct FrameDisplay

/// Specifies the number of stack frames that the call stack can contain before a stack overflow occurs.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Size(std::num::NonZeroUsize);

impl Size {
    pub const fn new(size: std::num::NonZeroUsize) -> Self {
        Self(size)
    }

    pub const fn get(self) -> std::num::NonZeroUsize {
        self.0
    }

    pub const DEFAULT: Self = Self::new(unsafe {
        // Safety: The value below is not zero.
        std::num::NonZeroUsize::new_unchecked(0xFFFFF)
    });
}

/// The SAILAR virtual machine call stack.
pub struct Stack {
    // Frames are boxed since popping and pushing happens regularly
    #[allow(clippy::vec_box)]
    frames: Vec<Box<Frame>>,
    size: Size,
}

impl Stack {
    pub(crate) fn with_size(size: Size) -> Self {
        Self {
            frames: Default::default(),
            size,
        }
    }

    /// Returns an iterator over the call stack, yielding the most recently pushed frames first.
    pub fn iter_frames(&self) -> impl std::iter::ExactSizeIterator<Item = &Frame> {
        self.frames.iter().map(std::borrow::Borrow::borrow).rev()
    }

    pub fn is_execution_ended(&self) -> bool {
        self.frames.is_empty()
    }

    /// Pops the current stack frame.
    ///
    /// # Panics
    ///
    /// Panics when the call stack is empty.
    pub(crate) fn pop(&mut self) -> Box<Frame> {
        self.frames.pop().expect("call stack underflow")
    }

    pub(crate) fn push(&mut self, frame: Box<Frame>) {
        self.frames.push(frame);
    }

    pub(crate) fn push_new(&mut self, callee: runtime::Function, arguments: Box<[value::Value]>) -> runtime::Result<()> {
        if self.frames.len() == self.size.get().get() {
            todo!("error stack overflow")
        }

        self.push(Box::new(Frame {
            kind: match callee.template()?.as_definition().unwrap().body()? {
                sailar_load::function::Body::Defined(code) => FrameKind::Defined(DefinedFrame::new(arguments, code.clone())?),
            },
            function: callee,
        }));

        Ok(())
    }
}

impl Debug for Stack {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.iter_frames()).finish()
    }
}
