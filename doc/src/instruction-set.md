# SAILAR Instruction Set

The following table shows the instructions corresponding to each opcode. If no instruction is defined for an opcode, then it is not valid.

## Value Encoding
Values in SAILAR binaries start with a flag byte indicating the type of value to follow.

Bit|Name|Notes
---|---|---
`0`|Constant|If set, indicates that the value is a constant. Otherwise, an index integer to a register follows the flag byte, and the remaining bits are not set.
`1`|Integer|If set, indicates that the constant value is an integer. As no other constants currently exist, this is always set.

The following flags apply for integer constants:

Bit|Name|Nptes
---|---|---
`2`|Sign|If set, indicates that the constant integer is signed.
`3-4`|Size|Indicates the size of the constant integer. `0` is 1 byte, `1` is 2 bytes, `2` is 4 bytes, and `3` is 8 bytes.
`5-7`|Unused|Must not be set.

## Instruction Table

Opcode|Instruction
---|---
`0x00`|[`nop`](#nop)
`0x01`|[`break`](#break)

## `nop`
```text
nop
```
Does absolutely nothing.

## `break`
```text
break
```
In interpreters and runtimes that support, indicates that a breakpoint has been hit. If not supported, functions exactly like a [`nop`](#nop) instruction.
