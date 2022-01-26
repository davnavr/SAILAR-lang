#[derive(Default)]
pub struct Cache {
    line_buffer: String,
}

impl Cache {
    // TODO: Properly handle quoted strings.
    pub fn read_command(&mut self) -> Vec<&str> {
        // Line and argument buffers are not cached, though this probably doesn't impact performance.
        self.line_buffer.clear();
        std::io::stdin().read_line(&mut self.line_buffer).unwrap();

        self.line_buffer.split_whitespace().collect()
    }
}
