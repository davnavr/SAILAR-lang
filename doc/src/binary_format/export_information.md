# Export Information

All SAILAR modules may contain definitions such as globals, [functions templates](./records/function_template.md), etc. that can
be exposed to other modules.

Export information for these definitions follow a common structure:

- A [variable length integer](./values.md#variable-length-integers)
  - The lowest bit of the value, if **set**, indicates that the definition is exported and that the following name cannot be empty
  - The remaining bits contain the **byte length** of the symbol name
- A symbol name, which is simply a sequence of bytes containing valid UTF-8 and follows the same constraints as [identifier records](./records/identifier.md)
