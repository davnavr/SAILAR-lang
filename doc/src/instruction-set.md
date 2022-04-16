# SAILAR Instruction Set

The following table shows the instructions corresponding to each opcode. If no instruction is defined for an opcode, then it is not valid.

Opcode|Instruction
---|---
`0x00`|[`nop`](#nop)
`0x01`|[`break`](#break)

# `nop`
```text
nop
```
Does absolutely nothing.

# `break`
```text
break
```
In interpreters and runtimes that support, indicates that a breakpoint has been hit. If not supported, functions exactly like a [`nop`](#nop) instruction.
