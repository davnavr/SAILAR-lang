//! Module for interacting with the SAILAR virtual machine's call stack.

use crate::runtime;
use crate::value;
use sailar_load::code_block::Instruction;
use std::fmt::{Debug, Formatter};

type CodeBlock = std::sync::Arc<sailar_load::code_block::Code>;

#[derive(Clone, Debug)]
pub struct CodeBlockLocation {
    block: CodeBlock,
    index: usize,
}

impl CodeBlockLocation {
    fn new(block: CodeBlock) -> Self {
        Self { block, index: 0 }
    }

    pub(crate) fn next_instruction(&mut self) -> runtime::Result<Option<&Instruction>> {
        let instruction = self.block.instructions()?.get(self.index);
        if instruction.is_some() {
            self.index += 1;
        }
        Ok(instruction)
    }
}

#[derive(Clone, Debug)]
pub enum FrameLocation {
    Defined(CodeBlockLocation),
    //Foreign(),
}

#[derive(Clone, Debug)]
pub struct Frame {
    function: runtime::Function,
    arguments: Box<[value::Value]>,
    location: FrameLocation,
}

impl Frame {
    pub fn location(&self) -> &FrameLocation {
        &self.location
    }

    pub(crate) fn location_mut(&mut self) -> &mut FrameLocation {
        &mut self.location
    }
}

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

    pub(crate) fn push(&mut self, callee: runtime::Function, arguments: Box<[value::Value]>) -> runtime::Result<()> {
        if self.frames.len() == self.size.get().get() {
            todo!("error stack overflow")
        }

        self.frames.push(Box::new(Frame {
            arguments,
            location: match callee.template()?.as_definition().unwrap().body()? {
                sailar_load::function::Body::Defined(code) => FrameLocation::Defined(CodeBlockLocation::new(code.clone())),
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
