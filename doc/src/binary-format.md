# SAILAR Binary Format

Describes the layout of SAILAR modules (`.sail` files)

# Module

Placeholder Text contains functions, structs, globals, code blocks, runtimes (name to give to htings that add to function bodies), address spaces, exception classes, etc.

### Structure
Offset|Name|Description
---|---|---
0|Magic|The magic number of modules, which is the ASCII string `SAILAR\0\0`
6|Major Format Version|The major version of the format, changes to this number are not backwards compatible
7|Minor format Version|The minor version of the format

## Struct

Represents a set of fields which form a type.

### Structure
Offset|Bits|Name|Notes
---|---|---|---
0|0|Exported|Indicates if this structure is visible to other modules
0|1|Name for equivalent of Rust's non-exhaustive|Indicates that all fields are visible to other modules
0|2-7|Reserved|Should not be set
