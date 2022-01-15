use registir::format::Identifier;
use runmdl::interpreter::{debugger, Interpreter};
use std::{fmt::Write as _, io::Write as _};

mod commands;

struct InputCache {
    line_buffer: String,
}

impl InputCache {
    fn read_command(&mut self) -> Option<(&str, Vec<&str>)> {
        // Line and argument buffers are not cached, though this probably doesn't impact performance.
        self.line_buffer.clear();
        std::io::stdin().read_line(&mut self.line_buffer).unwrap();

        let mut arguments = self.line_buffer.split_whitespace();

        arguments.next().map(|command_name| {
            let mut v = {
                let (min_capacity, max_capacity) = arguments.size_hint();
                Vec::with_capacity(max_capacity.unwrap_or(min_capacity))
            };
            v.extend(arguments);
            (command_name, v)
        })
    }
}

#[derive(Debug)]
pub struct Location<'a>(pub &'a debugger::InstructionLocation);

impl std::fmt::Display for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self(location) = self;
        write!(
            f,
            "at block {} index {}",
            location.block_index, location.code_index
        )
    }
}

#[derive(Debug)]
pub struct ModuleSymbol<'a>(pub &'a debugger::ModuleIdentifier);

impl std::fmt::Display for ModuleSymbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.name)?;
        let version = &self.0.version;
        if version.is_empty() {
            f.write_str(", v")?;
            for (i, number) in version.0.into_iter().enumerate() {
                if i > 0 {
                    f.write_char('.')?;
                }
                std::fmt::Display::fmt(&number, f)?;
            }
        }
        Ok(())
    }
}

pub struct CommandLineDebugger {
    started: bool,
    commands: commands::Lookup,
    input_buffer: InputCache,
}

impl CommandLineDebugger {
    pub fn new() -> Self {
        let mut commands = std::collections::BTreeMap::new();
        let mut command_name_width = 0usize;

        macro_rules! command {
            ($name: expr, $description: expr, $command: expr) => {{
                if $name.len() > command_name_width {
                    command_name_width = $name.len();
                }

                commands.insert(
                    $name,
                    commands::Command {
                        description: $description,
                        command: &$command,
                    },
                );
            }};
        }

        command!("help", "lists all commands", |commands, _, _| {
            for (name, commands::Command { description, .. }) in &commands.commands {
                println!(
                    "{:width$} - {}",
                    name,
                    description,
                    width = commands.name_width
                );
            }

            Ok(None)
        });

        command!(
            "detach",
            "stops debugging and continues execution of the program",
            |_, _, _| { Ok(Some(debugger::Reply::Detach)) }
        );

        command!(
            "break",
            "sets a breakpoint in the specified function",
            |_, arguments, interpreter| {
                // TODO: Allow selecting of module to lookup function in.
                let function_name =
                    arguments
                        .first()
                        .ok_or("missing function name")
                        .and_then(|name| {
                            Identifier::try_from(*name).map_err(|_| "empty function name")
                        })?;

                let matches = interpreter
                    .loader()
                    .lookup_function(debugger::Symbol::Owned(function_name));

                match matches.first() {
                    Some(function) if matches.len() == 1 => {
                        todo!();
                        Ok(None)
                    }
                    Some(_) => Err("multiples matches for function symbol")?,
                    None => Err("no function found with symbol")?
                }
            }
        );

        command!("points", "lists all breakpoints", |_, _, interpreter| {
            for breakpoint in interpreter.call_stack().breakpoints_mut().iter() {
                let function = breakpoint.function();
                println!(
                    "- {} from {} {}",
                    function.symbol(),
                    ModuleSymbol(function.module()),
                    Location(breakpoint.location())
                );
            }

            Ok(None)
        });

        // command!("where", "prints a stack trace", |_, _, interpreter| {
        //     let frames = interpreter.call_stack().stack_trace();
        //     Ok(debugger::Reply::Wait)
        // });

        Self {
            started: false,
            commands: commands::Lookup {
                commands,
                name_width: command_name_width,
            },
            input_buffer: InputCache {
                line_buffer: String::new(),
            },
        }
    }
}

impl debugger::Debugger for CommandLineDebugger {
    fn inspect(&mut self, interpreter: &mut Interpreter) -> debugger::Reply {
        if !self.started {
            println!("Type 'help' for help, or 'cont' to begin program execution");
            self.started = true;
        }

        loop {
            print!("> ");
            std::io::stdout().flush().unwrap();

            if let Some((name, arguments)) = self.input_buffer.read_command() {
                if let Some(command) = self.commands.commands.get(name) {
                    if let Some(reply) = command.execute(&self.commands, &arguments, interpreter) {
                        return reply;
                    }
                } else {
                    eprintln!("'{}' is not a valid command", name);
                }
            }
        }
    }
}
