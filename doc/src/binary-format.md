# SAILAR Binary Format
Describes the layout of SAILAR modules (`.sail` files)

## Module
TODO Placeholder Text contains functions, structs, globals, code blocks, runtimes (name to give to htings that add to function bodies), address spaces, exception classes, etc.

Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Magic|`6`|The magic number that all module files must begin with, which is the 6-byte long ASCII string `SAILAR\0\0`.
`6`|Major Format Version|`1`|The major version of the format, changes to this number are not backwards compatible.
`7`|Minor Format Version|`1`|The minor version of the format.
`8`|[Length Size](#Length-Size)|`1`|
`9`|[Module Header](#Module-Header)|`H`|
`9 + H`|

## Length Size
The length size indicates the size (`L`) of unsigned integers used to denote lengths, all values not listed below are invalid.

Value|Length Size (in bytes)
---|---
`0`|`1`
`1`|`2`
`2`|`4`

## Identifiers
TODO Figure out if arbitrary sequences of bytes are allowed or if UTF-8 is required. Identifiers are not null terminated and also cannot contain any null (`\0`) bytes.

Offset|Name|Notes
---|---|---
`0`|Length|A non-zero [length integer](#Length-Size) indicating the length of the identifier, in bytes.
`L`|Characters|

## Module Identifier
Identifies a module by its name and version.
Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Name|`L + N`|An [identifier](#Identifier) containing `N` characters that is the name of the module.
`L + N`|Version Number Count|`L`|The number `V` of version numbers to follow.
`2L + N`|Version Numbers|`L * V`|An array of [length integers](#Length-Size) specifying the version of the module.

## Module Header
Contains information that describes the module.
Offset|Name|Size|Notes
---|---|---|---
`0`|Size|`L`|A non-zero [length integer](#Length-Size) indicating the total length of the values of each field in the header to follow, in bytes.
`L`|Identifier|`I = 2L + N + V * L`|Specifies a name containing `N` characters and `V` version numbers for the module.
`I`|Optional Field Count|`L`|A [length integer](#Length-Size) indicating an additional number of fields to follow. As no optional fields are currently defined, this should be set to zero.

## Struct
Represents a set of fields which form a type.

Offset|Bits|Name|Notes
---|---|---|---
`0`|`0`|Export|Indicates if this structure is visible to other modules.
`0`|`1`|Name for equivalent of Rust's non-exhaustive|Indicates that all fields are visible to other modules. Can only be set if all fields are exported. This can be used to indicate that valid instances of this struct cannot be made as all fields are not known.
`0`|`2..7`|Reserved|Must not be set.
