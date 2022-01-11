use std::collections::hash_map;
use std::sync::mpsc;

pub use crate::interpreter::{
    BlockIndex, InstructionLocation, LoadedFunction, Register, StackTrace,
};
pub use getmdl::loader::ModuleIdentifier;

#[derive(Default)]
pub struct Breakpoint {
    pub block: BlockIndex,
    pub instruction: usize,
    //pub method: Option<FullMethodIdentifier>,
}

impl Breakpoint {
    pub fn instruction_location(&self) -> InstructionLocation {
        InstructionLocation {
            block_index: self.block,
            code_index: self.instruction,
        }
    }
}

impl std::fmt::Display for Breakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let location = self.instruction_location();
        // if let Some(ref method_name) = self.method {
        //     write!(f, "{} ", method_name)?;
        // }

        write!(
            f,
            "at block {} instruction {}",
            location.block_index.to_raw(),
            location.code_index
        )
    }
}

pub enum MessageReply {
    StackTrace(StackTrace), // TODO: Use Rc<Vec> in message reply to reduce heap allocations?
    //Breakpoints(Vec<Breakpoint>),
    Registers(Vec<Register>), // TODO: Also include input registers.
}

pub enum MessageKind {
    Continue,
    //SetBreakpoint(Breakpoint),
    //GetBreakpoints,
    GetStackTrace,
    GetRegisters, // TODO: Allow selection of stack frame to show registers for.
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

pub(crate) type BreakpointsReference = std::rc::Rc<std::cell::RefCell<Box<Vec<usize>>>>;

struct BlockBreakpoints {
    sorted: std::cell::Cell<bool>,
    indices: BreakpointsReference,
}

impl BlockBreakpoints {
    fn new() -> Self {
        Self {
            sorted: std::cell::Cell::new(true),
            indices: std::rc::Rc::default(),
        }
    }

    fn insert(&self, index: usize) {
        // TODO: Check for duplicate breakpoints in the same instruction.
        let mut indices = self.indices.borrow_mut();
        match indices.last() {
            Some(last) if *last <= index => (),
            _ => self.sorted.set(false),
        }
        indices.push(index);
    }

    fn indices(&self) -> BreakpointsReference {
        if !self.sorted.get() {
            self.indices.borrow_mut().sort_unstable();
            self.sorted.set(true);
        }

        self.indices.clone()
    }
}

pub(crate) struct Debugger<'l> {
    breakpoints:
        hash_map::HashMap<LoadedFunction<'l>, hash_map::HashMap<BlockIndex, BlockBreakpoints>>,
    receiver: MessageReceiver,
}

impl<'l: 'd, 'd> Debugger<'l> {
    pub(crate) fn new(receiver: MessageReceiver) -> Self {
        Self {
            breakpoints: hash_map::HashMap::new(),
            receiver,
        }
    }

    pub(crate) fn receive_message(&self) -> Result<Message, mpsc::RecvError> {
        self.receiver.recv()
    }

    // /// Retrieves the breakpoints in the specified code block in ascending order.
    // pub(crate) fn breakpoints_in_block(
    //     &'d self,
    //     method: LoadedFunction<'l>,
    //     block: BlockIndex,
    // ) -> Option<BreakpointsReference> {
    //     self.breakpoints.get(method).and_then(|block_breakpoints| {
    //         block_breakpoints.get(&block).map(BlockBreakpoints::indices)
    //     })
    // }

    // pub(crate) fn set_breakpoint(
    //     &mut self,
    //     method: LoadedMethod<'l>,
    //     location: &InstructionLocation,
    // ) {
    //     self.breakpoints
    //         .entry(method)
    //         .or_insert_with(hash_map::HashMap::new)
    //         .entry(location.block_index)
    //         .or_insert_with(BlockBreakpoints::new)
    //         .insert(location.code_index)
    // }

    // pub(crate) fn breakpoints<'a>(&'a self) -> Vec<Breakpoint> {
    //     let mut breakpoints = Vec::new();
    //     for (&method, block_breakpoints) in self.breakpoints.iter() {
    //         for (&block, code_indices) in block_breakpoints {
    //             for &index in code_indices.indices().borrow().iter() {
    //                 breakpoints.push(Breakpoint {
    //                     block,
    //                     instruction: index,
    //                     method: Some(method.identifier().unwrap()),
    //                 });
    //             }
    //         }
    //     }
    //     breakpoints
    // }
}
