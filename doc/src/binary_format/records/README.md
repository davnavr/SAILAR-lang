# Records

All records stored in a [SAILAR module](../module.md) follow the same basic structure:
- A byte tag indicating what kind of contents are stored in the records
- A [variable length integer](../values.md#variable-length-integers) indicating the size of the record's contents, in bytes
- The actual contents of the record
