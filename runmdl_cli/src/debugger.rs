use runmdl::interpreter::{debugger, Interpreter};

type CommandResult = Result<debugger::Reply, String>;

#[derive(Clone, Copy)]
struct Command {
    description: &'static str,
    command:
        &'static dyn Fn(&mut CommandLineDebugger, &[&str], &mut Interpreter) -> CommandResult,
}

pub struct CommandLineDebugger<'a> {
    started: bool,
    commands: std::collections::HashMap<&'static str, Command>,
    line_buffer: String,
    argument_buffer: Vec<&'a str>,
}

impl<'a> CommandLineDebugger<'a> {
    pub fn new() -> Self {
        let mut commands = std::collections::HashMap::new();

        macro_rules! command {
            ($name: expr, $description: expr, $command: expr) => {
                commands.insert(
                    $name,
                    Command {
                        description: $description,
                        command: &$command,
                    },
                );
            };
        }

        command!("help", "lists all commands", |debugger, _, _| {
            for (name, Command { description, .. }) in &debugger.commands {
                println!("- {}, {}", name, description);
            }

            Ok(debugger::Reply::Wait)
        });

        Self {
            started: false,
            commands,
        }
    }
}

impl debugger::Debugger for CommandLineDebugger<'_> {
    fn inspect(&mut self, interpreter: &mut Interpreter) -> debugger::Reply {
        if !self.started {
            println!("Type 'help' for help, or 'cont' to begin program execution");
            self.started = true;
        }

        todo!();
        debugger::Reply::Detach
    }
}
