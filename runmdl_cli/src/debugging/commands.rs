use runmdl::interpreter::{debugger, Interpreter};

type ErrorMessage = std::borrow::Cow<'static, str>;

type Result = std::result::Result<Option<debugger::Reply>, ErrorMessage>;

// trait Action {
//    type Arguments;
// }

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
        arguments: &[&str],
        interpreter: &mut Interpreter,
    ) -> Option<debugger::Reply> {
        (self.command)(commands, arguments, interpreter).unwrap_or_else(|error| {
            eprintln!("Error: {}", error);
            None
        })
    }
}
