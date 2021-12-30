use crate::interpreter::LoadedMethod;
use std::sync::mpsc;

pub struct MessageReply;

#[derive(Debug)]
pub enum MessageKind {
    /// Starts the execution of the application's entry point.
    Start,
}

#[derive(Debug)]
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
}

#[derive(Eq, PartialEq, Hash)]
pub struct Breakpoint<'l> {
    method: LoadedMethod<'l>,
    block_index: usize,
    code_index: usize,
}

pub type MessageReceiver = std::sync::mpsc::Receiver<Message>;

pub(crate) struct Debugger<'l> {
    breakpoints: std::collections::HashSet<Breakpoint<'l>>,
    receiver: MessageReceiver,
}

impl<'l> Debugger<'l> {
    pub(crate) fn new(receiver: MessageReceiver) -> Self {
        Self {
            breakpoints: std::collections::HashSet::new(),
            receiver,
        }
    }

    pub(crate) fn receive_message(&self) -> Result<Message, mpsc::RecvError> {
        self.receiver.recv()
    }
}
