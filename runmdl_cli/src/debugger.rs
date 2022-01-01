use runmdl::interpreter::{debugger, debugger::FullIdentifier as _};
use std::{io, sync::mpsc};

type CommandResult = Result<(), String>;

#[derive(Clone, Copy)]
struct Command {
    description: &'static str,
    command: &'static dyn Fn(&mut Debugger, &[&str]) -> CommandResult,
}

struct Debugger {
    is_running: bool,
    commands: std::collections::HashMap<&'static str, Command>,
    channel: mpsc::SyncSender<debugger::Message>,
    reply_channel_sender: mpsc::SyncSender<debugger::MessageReply>,
    reply_channel_receiver: mpsc::Receiver<debugger::MessageReply>,
}

impl Debugger {
    fn send_message(&mut self, message: debugger::MessageKind) -> CommandResult {
        self.channel
            .send(debugger::Message::new(
                self.reply_channel_sender.clone(),
                message,
            ))
            .map_err(|_| "debugger was disconnected".to_string())
    }

    fn expect_reply(&mut self) -> debugger::MessageReply {
        self.reply_channel_receiver.recv().unwrap()
    }
}

fn all_commands() -> Vec<(&'static str, Command)> {
    vec![
        (
            "help",
            Command {
                description: "lists all commands",
                command: &|debugger, _| {
                    for (name, Command { description, .. }) in &debugger.commands {
                        println!("- {}, {}", name, description);
                    }
                    Ok(())
                },
            },
        ),
        (
            "quit",
            Command {
                description: "stops debugging and continues execution of the application",
                command: &|debugger, _| {
                    debugger.is_running = false;
                    Ok(())
                },
            },
        ),
        (
            "break",
            Command {
                description: "sets a breakpoint in the specified method",
                command: &|debugger, arguments| {
                    let method = match arguments.get(0) {
                        Some(name) => Some(
                            debugger::FullMethodIdentifier::parse(name)
                                .ok_or_else(|| String::from("invalid method name"))?,
                        ),
                        None => None,
                    };

                    let instruction = match arguments.get(2) {
                        Some(index) => str::parse(index)
                            .map_err(|_| String::from("invalid instruction index"))?,
                        None => 0,
                    };

                    debugger.send_message(debugger::MessageKind::SetBreakpoint(
                        debugger::Breakpoint {
                            block: debugger::BlockIndex(None),
                            instruction,
                            method,
                        },
                    ))
                },
            },
        ),
        (
            "points",
            Command {
                description: "lists all breakpoints",
                command: &|debugger, _| {
                    //expect_empty_arguments
                    debugger.send_message(debugger::MessageKind::GetBreakpoints)?;

                    match debugger.expect_reply() {
                        debugger::MessageReply::Breakpoints(breakpoints) => {
                            for point in breakpoints {
                                println!("- {}", point)
                            }
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                },
            },
        ),
        (
            "cont",
            Command {
                description: "continues execution until the next breakpoint is hit",
                command: &|debugger, _| {
                    //expect_empty_arguments
                    debugger.send_message(debugger::MessageKind::Continue)
                },
            },
        ),
        (
            "where",
            Command {
                description: "prints a stack trace",
                command: &|debugger, _| {
                    debugger.send_message(debugger::MessageKind::GetStackTrace)?;

                    match debugger.expect_reply() {
                        debugger::MessageReply::StackTrace(stack_frames) => {
                            for frame in stack_frames {
                                println!(
                                    "- {} at block {} instruction {}",
                                    frame.method(),
                                    frame.location().block_index.to_raw(),
                                    frame.location().code_index
                                )
                            }
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                },
            },
        ),
        (
            "temps",
            Command {
                description: "prints all temporary registers that have been defined",
                command: &|debugger, _| {
                    debugger.send_message(debugger::MessageKind::GetRegisters)?;

                    match debugger.expect_reply() {
                        debugger::MessageReply::Registers(temporaries) => {
                            for (index, register) in temporaries.iter().enumerate() {
                                println!("- t{} = {}", index, register,)
                            }
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                },
            },
        ),
    ]
}

const REPLY_CHANNEL_BOUND: usize = 0;

pub(crate) fn start(channel: mpsc::SyncSender<debugger::Message>) {
    let (reply_channel_sender, reply_channel_receiver) = mpsc::sync_channel(REPLY_CHANNEL_BOUND);
    let mut debugger = Debugger {
        is_running: true,
        commands: {
            let commands = all_commands();
            let mut lookup = std::collections::HashMap::with_capacity(commands.len());
            for (name, processor) in commands.iter() {
                if let Some(_) = lookup.insert(*name, *processor) {
                    unreachable!("Duplication definition for debugger command '{}'", name);
                }
            }
            lookup
        },
        channel,
        reply_channel_sender,
        reply_channel_receiver,
    };

    println!("Type 'help' for help, or 'cont' to begin program execution");
    while debugger.is_running {
        let mut line_buffer = String::new();
        io::stdin().read_line(&mut line_buffer).unwrap();
        let argument_buffer: Vec<_> = line_buffer.split_whitespace().collect();

        if let Some(command_name) = argument_buffer.first() {
            if let Some(command) = debugger.commands.get(command_name) {
                if let Err(error) = (command.command)(&mut debugger, &argument_buffer[1..]) {
                    eprintln!("Error: {}", error);
                }
            } else {
                eprintln!("'{}' is not a valid command", command_name);
            }
        }
    }
}
