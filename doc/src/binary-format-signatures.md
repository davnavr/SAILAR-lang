# SAILAR Binary Format Signatures

## Type
Denotes a type of a field, global, return value, argument, etc.

First Byte|Description
---|---
`0x01`|An unsigned 8-bit integer
`0x02`|An unsigned 16-bit integer
`0x04`|An unsigned 32-bit integer
`0x08`|A signed 64-bit integer
`0x11`|A signed 8-bit integer
`0x12`|A signed 16-bit integer
`0x14`|A signed 32-bit integer
`0x18`|A signed 64-bit integer
`0xF4`|32-bit floating-point
`0xF8`|64-bit floating-point
