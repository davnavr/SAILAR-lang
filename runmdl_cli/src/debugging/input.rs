#[derive(Default)]
pub struct Cache {
    line_buffer: String,
}

impl Cache {
    // TODO: Properly handle quoted strings.
    pub fn read_command(&mut self) -> Option<(&str, Vec<&str>)> {
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
