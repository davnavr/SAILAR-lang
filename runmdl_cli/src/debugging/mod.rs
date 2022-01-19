use clap::Parser as _;
use registir::format::Identifier;
use runmdl::interpreter::{call_stack, debugger, BlockIndex, InstructionLocation, Interpreter};
use std::{fmt::{Display, Formatter, Write as _}, io::Write as _};

mod commands;
mod input;

#[derive(Debug)]
pub struct Location<'a>(pub &'a debugger::InstructionLocation);

impl Display for Location<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
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

impl Display for ModuleSymbol<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.name)?;
        let version = &self.0.version;
        if version.is_empty() {
            f.write_str(", v")?;
            for (i, number) in version.0.into_iter().enumerate() {
                if i > 0 {
                    f.write_char('.')?;
                }
                Display::fmt(&number, f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionSymbol<'a>(pub &'a debugger::FunctionSymbol<'static>);

impl Display for FunctionSymbol<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "\"{}\" from {}", self.0.symbol(), ModuleSymbol(self.0.module()))
    }
}

fn print_registers(prefix: char, registers: &[debugger::Register]) {
    for (index, register) in registers.iter().enumerate() {
        println!("  %{}{} = {}", prefix, index, register);
    }
}

pub struct CommandLineDebugger {
    started: bool,
    commands: commands::Lookup,
    input_buffer: input::Cache,
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
            |_, command, interpreter| {
                #[derive(clap::Parser, Debug)]
                #[clap(
                    name = "break",
                    about,
                    global_setting(clap::AppSettings::DisableHelpFlag)
                )]
                // TODO: COuld have get and set subcommands and have set be the "main" one.
                struct Arguments {
                    /// The symbol name of the function to set a breakpoint in.
                    #[clap(short, long, parse(try_from_str = std::convert::TryFrom::try_from), value_name = "SYMBOL")]
                    function: Identifier,
                    #[clap(short, long, default_value_t)]
                    block: usize,
                    #[clap(short, long, default_value_t)]
                    instruction: usize,
                }

                let arguments = Arguments::try_parse_from(command)?;

                let matches = interpreter
                    .loader()
                    .lookup_function(debugger::Symbol::Owned(arguments.function));

                match matches.first() {
                    Some(function) if matches.len() == 1 => {
                        interpreter.call_stack().breakpoints_mut().insert(
                            call_stack::Breakpoint::new(
                                InstructionLocation {
                                    block_index: debugger::BlockIndex(if arguments.block == 0 {
                                        None
                                    } else {
                                        Some(arguments.block - 1)
                                    }),
                                    code_index: arguments.instruction,
                                },
                                function.full_symbol()?.to_owned(),
                            ),
                        );

                        Ok(None)
                    }
                    Some(_) => Err("multiples matches for function symbol")?,
                    None => Err("no function found with symbol")?,
                }
            }
        );

        command!("points", "lists all breakpoints", |_, _, interpreter| {
            for breakpoint in interpreter.call_stack().breakpoints_mut().iter() {
                let function = breakpoint.function();
                println!(
                    "- {} {}",
                    FunctionSymbol(function),
                    Location(breakpoint.location())
                );
            }

            Ok(None)
        });

        command!("where", "prints a stack trace", |_, command, interpreter| {
            #[derive(clap::Parser, Debug)]
            #[clap(
                name = "where",
                about,
                global_setting(clap::AppSettings::DisableHelpFlag)
            )]
            struct Arguments {
                #[clap(long)]
                show_registers: bool,
            }

            let arguments = Arguments::try_parse_from(command)?;

            for frame in interpreter.call_stack().stack_trace() {
                println!("- {} {}", FunctionSymbol(frame.function()), Location(frame.location()));
                if arguments.show_registers {
                    print_registers('i', frame.input_registers());
                    print_registers('t', frame.temporary_registers());
                }
            }
            Ok(None)
        });

        command!("cont", "continues execution of the program", |_, _, _| {
            Ok(Some(debugger::Reply::Continue))
        });

        Self {
            started: false,
            commands: commands::Lookup {
                commands,
                name_width: command_name_width,
            },
            input_buffer: input::Cache::default(),
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

            let input = self.input_buffer.read_command();
            if let Some(name) = input.first() {
                if let Some(command) = self.commands.commands.get(name) {
                    if let Some(reply) = command.execute(&self.commands, &input, interpreter) {
                        return reply;
                    }
                } else {
                    eprintln!("'{}' is not a valid command", name);
                }
            }
        }
    }
}
