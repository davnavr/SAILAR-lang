//! Module for interacting with the SAILAR virtual machine's call stack.

use crate::value;
use std::fmt::{Debug, Formatter};

type CodeBlock = std::sync::Arc<sailar_load::code_block::Code>;

#[derive(Clone, Debug)]
pub struct CodeBlockLocation {
    block: CodeBlock,
    index: usize,
}

impl CodeBlockLocation {
    fn new(block: CodeBlock) -> Self {
        Self {
            block,
            index: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub enum FrameLocation {
    Defined(CodeBlockLocation),
    //Foreign(),
}

#[derive(Clone, Debug)]
pub struct Frame {
    function: crate::Function,
    arguments: Box<[value::Value]>,
    location: FrameLocation,
}

/// The SAILAR virtual machine call stack.
pub struct Stack {
    frames: Vec<Frame>, //RefCell<Frame>
}

impl Stack {
    pub(crate) fn new() -> Self {
        Self { frames: Vec::new() }
    }

    /// Returns an iterator over the call stack, yielding the most recently pushed frames first.
    pub fn iter_frames(&self) -> impl std::iter::ExactSizeIterator<Item = &Frame> {
        self.frames.iter().rev()
    }

    pub(crate) fn peek(&self) -> &Frame {
        self.frames.last().expect("call stack must never be empty")
    }

    // TODO: Handle return values when popping frame.
    pub(crate) fn pop(&mut self) {
        self.frames.pop().expect("call stack underflow");
    }

    pub(crate) fn push(&mut self, callee: crate::Function, arguments: Box<[value::Value]>) -> crate::Result<()> {
        self.frames.push(Frame {
            arguments,
            location: match callee.template()?.as_definition().unwrap().body()? {
                sailar_load::function::Body::Defined(code) => FrameLocation::Defined(CodeBlockLocation::new(code.clone())),
            },
            function: callee,
        });

        Ok(())
    }
}

impl Debug for Stack {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.iter_frames()).finish()
    }
}
