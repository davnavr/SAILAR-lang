# Modules

All SAILAR modules begin with a preamble containing:
- The 6 byte long magic number (the ASCII string `SAILAR`)
- A major version byte and minor version byte indicating the version of the SAILAR format being used
- A [variable length integer](./values.md#variable-length-integers) indicating the number of records contained in the module

Following the preamble are the [records](./records.md) that make up the contents of the module.
