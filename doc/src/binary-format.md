# SAILAR Binary Format
Describes the layout of SAILAR modules (`.sail` files)

## Module
TODO Placeholder Text contains functions, structs, globals, code blocks, runtimes (name to give to things that add to function bodies), address spaces, exception classes, etc.

Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Magic|`6`|The magic number that all module files must begin with, which is the ASCII string `SAILAR`.
`6`|Major Format Version|`1`|The major version of the format, changes to this number are not backwards compatible.
`7`|Minor Format Version|`1`|The minor version of the format.
`8`|[Length Size](#length-size)|`1`
`9`|[Module Header](#module-header)|?
|?|[Module Identifiers](#module-identifiers)|?
|?|[Type Signatures](#type-signatures)|?
|?|[Function Signatures](#function-signatures)|?
|?|[Data](#module-data)|?
|?|[Code Blocks](#code)|?
|?|[Module Imports](#module-imports)|?
|?|[Module Definitions](#module-definitions)|?
|?|[Struct Instantiations](#struct-instantiations)|?
|?|[Function Instantiations](#function-instantiations)|?
|?|[Entry Point](#entry-point)|?
|?|[Module Initializer](#module-initializer)|?
|?|[Namespaces](#namespaces)|?
|?|[Debugging Information]|?

## Length Size
The length size indicates the size (`L`) of unsigned integers used to denote lengths and indices, all values not listed in the table below are invalid.

Value|Length Integer Size (in bytes)
---|---
`0`|`1`
`1`|`2`
`2`|`4`

## Identifiers
Identifiers are valid UTF-8 strings that are length-prefixed and not null-terminated. Valid identifiers must not have a length of zero and cannot contain any null (`\0`) code points.

Offset|Name|Notes
---|---|---
`0`|Length|A non-zero [length integer](#length-size) indicating the length of the identifier, in bytes.
`L`|Characters|

## Symbols
All definitions in a module (structs, functions, globals, etc.) have an [identifier](#identifiers) known as a symbol.
All definitions in a module have a unique symbol, meaning that no two definitions in a module can have the same symbol.

## Module Identifier
Identifies a module by its name and version.

Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Name|`L + N`|An [identifier](#identifier) containing `N` characters that is the name of the module.
`L + N`|Version Number Count|`L`|The number `V` of version numbers to follow.
`2L + N`|Version Numbers|`L * V`|An array of [length integers](#length-size) specifying the version of the module.

## Module Header
Contains information that describes the module.

Offset|Name|Size|Notes
---|---|---|---
`0`|Size|`L`|A non-zero [length integer](#length-size) indicating the total length of the values of each field to follow, in bytes.
`L`|[Identifier](#module-identifier)|`I = 2L + N + V * L`|Specifies a name containing `N` characters and `V` version numbers for the module.
`I`|Optional Field Count|`L`|A [length integer](#length-size) indicating an additional number of fields to follow. As no optional fields are currently defined, this should be set to zero.

## Module Identifiers
Provides a way to use duplicated [identifiers](#identifier) without having to copy and paste their contents all over the module. Individual identifiers are referred to by [length-sized indices](#length-size) starting at `0`.

Offset|Name|Size|Notes|Omission
---|---|---|---|---
`0`|Size|`L`|A [length integer](#length-size) indicating the size `S` of all of the identifiers, in bytes.
`L`|Count|`L`|A [length integer](#length-size) indicating the number `C` of identifiers to follow.|Omitted when `Size` is zero
`2L`|Signatures|`S`|A series of [identifiers](#identifier).

## Type Signatures
All of the type signatures used in the module. Individual type signatures are referred to by [length-sized indices](#length-size) with the first type signature referred to by index `0`.

Offset|Name|Size|Notes|Omission
---|---|---|---|---
`0`|Size|`L`|A [length integer](#length-size) indicating the size `S` of the type signatures, in bytes, excluding the count.
`L`|Count|`L`|A [length integer](#length-size) indicating the number `C` of type signatures to follow.|Omitted when `Size` is zero
`2L`|Signatures|`S`|A series of [type signatures](./binary-format-signatures.md#type).

## Function Signatures
All of the function signatures used in the module. Individual function signatures are referred to by [length-sized indices](#length-size) with the first function signature referred to by index `0`.

Offset|Name|Size|Notes|Omission
---|---|---|---|---
`0`|Size|`L`|A [length integer](#length-size) indicating the size `S` of the function signatures, in bytes, excluding the count.
`L`|Count|`L`|A [length integer](#length-size) indicating the number `C` of function signatures to follow.|Omitted when `Size` is zero
`2L`|Signatures|`S`|A series of [function signatures](./binary-format-signatures.md#function).

## Module Data
Modules are allowed to contain arbitrary byte arrays that can be used for storing literal strings, the initial values of global variables, etc. Data arrays can be referred to by [length-sized indices](#length-size) starting at `0`.

Offset|Name|Notes|Omission
---|---|---|---
`0`|Total Size|A [length integer](#length-size) indicating the total size, in bytes, of all the data arrays combined.
`L`|Count|A [length integer](#length-size) indicating the total number `C` of data arrays to follow.|Omitted when `Total Size` is zero.
`2L`|Contents|A series of `C` [data arrays](#module-data-arrays) one after the other.

## Code
Code in SAILAR is broken into individual sequences of instructions known as blocks. Function definitions can specify their bodies by specifying the entry block which is the block that is entered when the function is called. [Refer to the instruction set documentation](./instruction-set.md) for the rules regarding valid blocks.

Offset|Notes|Omission
---|---|---
`0`|Total Size|A [length integer](#length-size) indicating the total size, in bytes, of all the code blocks combined.
`L`|Count|A [length integer](#length-size) indicating the total number of code blocks to follow.
`2L`|Code Blocks|A series of [code blocks](#code-block).

### Code Block
Each block describes its inputs, outputs, the types of its registers, and contains its instructions.

Offset|Notes|Omission|
---|---|---
`0`|Input Count|A [length integer](#length-size) indicating the number of inputs to this block.
`L`|Result Count|A [length integer](#length-size) indicating the number of results returned by this block.
`2L`|Temporary Count|A [length integer](#length-size) indicating the number of temporary registers introduced by the instructions in this block.
`3L`|Register Types|A series of [type signature indices](#type-signatures) specifying the type of each input, result, and temporary register in that order.
?|Instruction Size|A [length integer](#length-size) indicating the size of the total size of the instructions in this block, in bytes. As empty blocks are not allowed, this must be greater than zero.
?|Instruction Count|A [length integer](#length-size) indicating the number of instructions in this block. This must be greater than zero.
?|Instructions|The instructions that make up the code block. Refer to the [instruction set reference](./instruction-set.md) for more information.

### Module Data Arrays
Offset|Name|Notes
---|---|---
`0`|Length|The length `L` of this data array, in bytes.
`L`|Data|The actual data, contains `L` bytes.

## Module Imports
This structure contains all structs, functions, globals, etc. that are used by the current module.

TODO: Have this be a list of module imports instead, with each ModuleImport struct containing functions, structs, globals, etc.

Name|Notes
---|---
Total Size|A [length integer](#length-size) indicating the total size, in bytes, of all of the following module import information. If zero, all following fields are omitted.
Function Count|
[Function Imports]()|
Struct Count|
[Struct Imports]()|
Global Count|
[Global Imports]()|
Exception Class Count|A [length integer](#length-size), set to zero as SAILAR's exception handling mechanism is still being defined.
Exception Class Imports|Currently empty.
Annotation Class Count|A [length integer](#length-size), set to zero as the semantics of annotations are still being decided.
Annotation Class Imports|Currently empty.

## Module Definitions
Contains the structs, functions, globals, etc. defined by the current module.

Name|Notes
---|---
Total Size|A [length integer](#length-size) indicating the total size, in bytes, of all of the following module definition information. If zero, all following fields are omitted
Function Count|
[Function Definitions](#function-definition)|
Struct Count|
[Struct Definitions](#struct-definition)|
Global Count|
[Global Definitions]()|
Exception Class Count|Currently empty.
Exception Class Definitions|A [length integer](#length-size), set to zero as SAILAR's exception handling mechanism is still being defined.
Annotation Class Count|Currently empty.
Annotation Class Definitions|A [length integer](#length-size), set to zero as the semantics of annotations are still being decided.

### Function Definition
Describes a function defined in the current module. When generics are supported, a field will be added that lists generic parameters.

Offset|Bits|Name|Notes
---|---|---|---
`0`|`0`|Export|If set, indicates if this structure is visible to other modules.
`0`|`1`|Foreign|If set, indicates that the body of this function is defined elsewhere. Typically used when dealing with foreign function interface bindings.
`0`|`2..7`|Reserved|These bits must not be set.
`1`||Signature|A [length integer index](#length-size) that indicates the [function's signature](#function-signatures).
`1 + L`||[Symbol](#symbols)
?||[Function Body](#function-body)

### Function Body
If the `Foreign` bit is not set, then the function body is simply a [length integer index](#length-size) to a [code block](#code-block).

Otherwise, the function body describes a foreign function:

Name|Notes
---|---
Library|An [length integer index](#length-size) to an [identifier](#identifier) that specifies the name of the library that the function is defined in.
Entry Point Name|An [identifier](#identifier) that is the name of the function defined in the `Library`.

### Struct Definition
Represents a set of fields which form a type.

Offset|Bits|Name|Notes
---|---|---|---
`0`|`0`|Export|Indicates if this structure is visible to other modules.
`0`|`1`|Name for equivalent of Rust's non-exhaustive|Indicates that all fields are visible to other modules. Can only be set if all fields are exported. This can be used to indicate that valid instances of this struct cannot be made as all fields are not known.
`0`|`2..7`|Reserved|These bits must not be set.

## Struct Instantiations
Allows referring to struct definitions and imports, while also allowing generic structs in the future.

Offset|Name|Notes
---|---|---
`0`|Total Size|
`L`|Count|
`2L`|

Offset|Name|Notes
---|---|---
`0`|Struct Index|A [length integer](#length-size) used to refer to a struct import or definition, where `0` refers to the first struct import, and `x` refers to the first struct definition, where `x` is the total number of struct imports.
`L`|Reserved|A reserved [length integer](#length-size), must be set to zero.

## Function Instantiations
Allows referring to function definitions and imports, while also allowing generic functions in the future.

Offset|Name|Notes
---|---|---
`0`|Total Size|
`L`|Count|
`2L`|

Offset|Name|Notes
---|---|---
`0`|Function Index|A [length integer](#length-size) used to refer to a function import or definition, where `0` refers to the first function import, and `x` refers to the first function definition, where `x` is the total number of function imports.
`L`|Reserved|A reserved [length integer](#length-size), must be set to zero.

## Entry Point
The entry point function is the function that is executed when a SAILAR application is run.

## Module Initializer
The module initializer is a function that takes no arguments and returns no values that is called before execution of any entry point function begins. It allows modules to initialize global variables and other global state before execution of other code. In order to prevent ambiguity regarding the execution of module initializers, the module initializer function is not allowed to make calls to any imported functions or write to or read values from any imported globals.

## Namespaces
Namespaces allow compilers to organize and attach human readable names to the structs, globals, and functions of a module.

## Debugging Information
Modules can optionally contain debugging information.

Offset|Name|Notes
---|---|---
`0`|Size|A [length integer](#length-size) indicating the size of the following debugging information in bytes.
`L`|Debugging Information|The debugging information, [whose format is documented here](./debug-format.md).
