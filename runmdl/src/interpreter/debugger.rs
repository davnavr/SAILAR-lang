use std::collections::hash_map;
use std::sync::mpsc;

pub use crate::interpreter::{BlockIndex, InstructionLocation, LoadedMethod, StackTrace};

pub struct Breakpoint {
    pub block: BlockIndex,
    pub instruction: Option<usize>,
    //pub method: Option<MethodName>
}

impl Breakpoint {
    pub fn instruction_location(&self) -> InstructionLocation {
        InstructionLocation {
            block_index: self.block,
            code_index: self.instruction.unwrap_or_default(),
        }
    }
}

impl std::fmt::Display for Breakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let location = self.instruction_location();
        write!(
            f,
            "at block {} instruction {}",
            location.block_index.to_raw(),
            location.code_index
        )
    }
}

pub enum MessageReply {
    StackTrace(StackTrace),
    Breakpoints(Vec<Breakpoint>),
}

pub enum MessageKind {
    /// Starts the execution of the application's entry point.
    Start,
    SetBreakpoint(Breakpoint),
    GetBreakpoints,
}

pub struct Message {
    reply_channel: mpsc::SyncSender<MessageReply>,
    message: MessageKind,
}

impl Message {
    pub fn new(reply_channel: mpsc::SyncSender<MessageReply>, message: MessageKind) -> Self {
        Self {
            reply_channel,
            message,
        }
    }

    pub fn message(&self) -> &MessageKind {
        &self.message
    }

    pub(crate) fn reply(&self, reply: MessageReply) {
        self.reply_channel.send(reply).unwrap()
    }
}

pub type MessageReceiver = std::sync::mpsc::Receiver<Message>;

struct BlockBreakpointsContents {
    sorted: bool,
    indices: Vec<usize>,
}

struct BlockBreakpoints(std::cell::RefCell<BlockBreakpointsContents>);

impl BlockBreakpoints {
    fn new() -> Self {
        Self(std::cell::RefCell::new(BlockBreakpointsContents {
            sorted: true,
            indices: Vec::new(),
        }))
    }

    fn insert(&self, index: usize) {
        // TODO: Check for duplicate breakpoints in the same instruction.
        let mut breakpoints = self.0.borrow_mut();
        match breakpoints.indices.last() {
            Some(last) if *last <= index => (),
            _ => breakpoints.sorted = false,
        }
        breakpoints.indices.push(index);
    }

    fn indices(&self) -> std::cell::Ref<Vec<usize>> {
        {
            let mut breakpoints = self.0.borrow_mut();
            if !breakpoints.sorted {
                breakpoints.indices.sort_unstable();
                breakpoints.sorted = true;
            }
        }

        std::cell::Ref::map(
            self.0.borrow(),
            |BlockBreakpointsContents { indices, .. }| indices,
        )
    }
}

pub(crate) struct Debugger<'l> {
    breakpoints:
        hash_map::HashMap<LoadedMethod<'l>, hash_map::HashMap<BlockIndex, BlockBreakpoints>>,
    receiver: MessageReceiver,
}

impl<'l> Debugger<'l> {
    pub(crate) fn new(receiver: MessageReceiver) -> Self {
        Self {
            breakpoints: hash_map::HashMap::new(),
            receiver,
        }
    }

    pub(crate) fn receive_message(&self) -> Result<Message, mpsc::RecvError> {
        self.receiver.recv()
    }

    pub(crate) fn breakpoints_in_block(
        &self,
        method: LoadedMethod<'l>,
        block: BlockIndex,
    ) -> Option<std::cell::Ref<Vec<usize>>> {
        self.breakpoints.get(method).and_then(|block_breakpoints| {
            block_breakpoints.get(&block).map(BlockBreakpoints::indices)
        })
    }

    pub(crate) fn set_breakpoint(
        &mut self,
        method: LoadedMethod<'l>,
        location: &InstructionLocation,
    ) {
        self.breakpoints
            .entry(method)
            .or_insert_with(hash_map::HashMap::new)
            .entry(location.block_index)
            .or_insert_with(BlockBreakpoints::new)
            .insert(location.code_index)
    }

    pub(crate) fn breakpoints<'a>(&'a self) -> Vec<Breakpoint> {
        let mut breakpoints = Vec::new();
        for (_, block_breakpoints) in self.breakpoints.iter() {
            for (block, code_indices) in block_breakpoints {
                for index in std::ops::Deref::deref(&code_indices.indices()) {
                    breakpoints.push(Breakpoint {
                        block: *block,
                        instruction: Some(*index),
                    });
                }
            }
        }
        breakpoints
    }
}
