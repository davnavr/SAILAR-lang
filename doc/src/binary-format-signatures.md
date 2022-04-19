# SAILAR Binary Format Signatures

## Type
Denotes a type of a field, global, return value, argument, etc.

First Byte|Assembly Name|Description
---|---|---
`0x01`|`u8`|An unsigned 8-bit integer
`0x02`|`u16`|An unsigned 16-bit integer
`0x04`|`u32`|An unsigned 32-bit integer
`0x08`|`u64`|A signed 64-bit integer
`0x11`|`s8`|A signed 8-bit integer
`0x12`|`s16`|A signed 16-bit integer
`0x14`|`s32`|A signed 32-bit integer
`0x18`|`s64`|A signed 64-bit integer
`0xF4`|`f32`|32-bit floating-point
`0xF8`|`f64`|64-bit floating-point

## Function
Specifies the return values and argument types of a function.

Offset|Size|Name|Notes
---|---|---|---
`0`|`L`|Return Type Count|Specifies the number `R` of values returned by the function.
`L`|`L`|Parameter Count|Specifies the number `A` of parameters.
`2L`|`L*R`|Return Types|An array of indices specifying the return types of the function.
`2L + L*R`|`L*A`|Parameter Types|An array of indices specifying the types of the function's parameters.
