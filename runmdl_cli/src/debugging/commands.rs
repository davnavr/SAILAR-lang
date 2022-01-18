use runmdl::interpreter::{debugger, Interpreter};
use std::borrow::Cow;

#[derive(thiserror::Error, Debug)]
pub(super) enum Error {
    #[error("{0}")]
    Message(Cow<'static, str>),
    #[error(transparent)]
    ArgumentError(#[from] clap::Error),
}

impl From<&'static str> for Error {
    fn from(message: &'static str) -> Self {
        Self::Message(Cow::Borrowed(message))
    }
}

impl From<String> for Error {
    fn from(message: String) -> Self {
        Self::Message(Cow::Owned(message))
    }
}

pub(super) type Result = std::result::Result<Option<debugger::Reply>, Error>;

#[derive(Clone, Copy)]
pub struct Command {
    pub(super) description: &'static str,
    pub(super) command: &'static dyn Fn(&Lookup, &[&str], &mut Interpreter) -> Result,
}

pub struct Lookup {
    pub(super) commands: std::collections::BTreeMap<&'static str, Command>,
    pub(super) name_width: usize,
}

impl Command {
    pub fn execute(
        &self,
        commands: &Lookup,
        input: &[&str],
        interpreter: &mut Interpreter,
    ) -> Option<debugger::Reply> {
        (self.command)(commands, input, interpreter).unwrap_or_else(|error| {
            eprintln!("Error: {}", error);
            None
        })
    }
}
