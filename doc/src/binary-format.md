# SAILAR Binary Format
Describes the layout of SAILAR modules (`.sail` files)

## Module
TODO Placeholder Text contains functions, structs, globals, code blocks, runtimes (name to give to things that add to function bodies), address spaces, exception classes, etc.

Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Magic|`6`|The magic number that all module files must begin with, which is the ASCII string `SAILAR`.
`6`|Major Format Version|`1`|The major version of the format, changes to this number are not backwards compatible.
`7`|Minor Format Version|`1`|The minor version of the format.
`8`|[Integer Size](#integer-size)|`1`
`9`|Record Count|`C`|The number of records that follow.
`9 + L`|[Records](#record)|?|

## Integer Size
The integer size indicates the size (`L`) of unsigned integers used to denote lengths and indices, all values not listed in the table below are invalid.

Value|Integer Size (in bytes)
---|---
`0`|`1`
`1`|`2`
`2`|`4`

## Identifiers
Identifiers are valid UTF-8 strings that are length-prefixed and not null-terminated. Valid identifiers must not have a length of zero and cannot contain any null (`\0`) code points.

Offset|Name|Notes
---|---|---
`0`|Length|A non-zero [integer](#integer-size) indicating the length of the identifier, in bytes.
`L`|Characters|

## Symbols
All definitions in a module (structs, functions, globals, etc.) have an [identifier](#identifiers) known as a symbol.
All definitions in a module have a unique symbol, meaning that no two definitions in a module can have the same symbol.

## Module Identifier
Identifies a module by its name and version.

Offset|Name|Size (in bytes)|Notes
---|---|---|---
`0`|Name|`L + N`|An [identifier](#identifiers) containing `N` characters that is the name of the module.
`L + N`|Version Number Count|`L`|The number `V` of version numbers to follow.
`2L + N`|Version Numbers|`L * V`|An array of [integers](#integer-size) specifying the version of the module.

## Record
Each module in SAILAR is broken into a series of records, which describe the content of the module.

Each record begins with the following fields:

Offset|Name|Size|Notes
---|---|---|---
`0`|Type|`1`|Indicates what kind of content is contained in this record.
`1`|Content Size|`L`|A non-zero [integer](#integer-size) indicating the size `S` of the record's content, in bytes.
`L + 1`|Content|`S`|The content of the record.

# Record Types

## Array Record
Allows one or more records of the same type to be contained in a single record. To simplify the format, nested arrays are not
allowed.

Offset|Name|Size|Notes
---|---|---|---
`0`|Record Type|`1`|Indicates the type of records contained in this array. Must not be an array itself.
`1`|Record Count|`L`|A non-zero [integer](#integer-size) indicating the number of records in the array.
`L + 1`|Records|?|The records in the array.

## Metadata Record
A record that describes an aspect of the module. The content of a metadata record starts with a single
[identifier](#identifiers) called the field name, which indicates the meaning of the field's contents.

Field Name|Description
---|---
`id`|Indicates that a [module identifier](#module-identifier) follows the field name, indicating the name and version of the current module.

## Identifier Record
Provides a way to use duplicated [identifiers](#identifiers) without having to copy and paste their contents all over the module.
Individual identifiers are referred to by [integer](#integer-size) indices starting at zero.

## Type Signature Record
Contains a single [type signature](./binary-format-signatures.md#type). Individual type signatures are referred to by
[integer](#integer-size) indices starting at zero.

## Function Signature Record
Contains a single [function signature](./binary-format-signatures.md#function). Individual function signatures are referred to by
[integer](#integer-size) indices starting at zero.

## Data Record
Modules are allowed to contain arbitrary byte arrays that can be used for storing literal strings, the initial values of global
variables, etc. Individual data arrays are referred to by [integer](#integer-size) indices starting at zero.

Offset|Name|Notes
---|---|---
`0`|Length|The length `L` of this data array, in bytes.
`L`|Data|The actual data, contains `L` bytes.

## Code Record
Code in SAILAR is broken into individual sequences of instructions known as blocks. Function definitions can specify their bodies
by specifying the entry block which is the block that is entered when the function is called.
[Refer to the instruction set documentation](./instruction-set.md) for the rules regarding valid blocks.

Each block describes its inputs, outputs, the types of its registers, and contains its instructions.

Offset|Name|Notes|
---|---|---
`0`|Input Count|An [integer](#integer-size) indicating the number of inputs to this block.
`L`|Result Count|An [integer](#integer-size) indicating the number of results returned by this block.
`2L`|Temporary Count|An [integer](#integer-size) indicating the number of temporary registers introduced by the instructions in this block.
`3L`|Register Types|A series of [type signature indices](#type-signatures) specifying the type of each input, result, and temporary register in that order.
?|Instruction Count|An [integer](#integer-size) indicating the number of instructions in this block. This must be greater than zero.
?|Instructions|The instructions that make up the code block. Refer to the [instruction set reference](./instruction-set.md) for more information.

## Module Import Record

Describes a module containing structs, functions, globals, and other definitions used by the current module. Imported modules are
referred to by [integer](#integer-size) indices starting at one, with an index of zero referring to the current module.

Offset|Name|Size|Notes
---|---|---|---
`0`|[Module Identifier](#module-identifier)|`H`|The name and verion of the imported module.
`H`|Flags|`1`|Set to `1` if a SHA-256 hash of the module to import is present, or `0` if no hash is present.

The following is present depending on the value of the `Flags` field.

Offset|Name|Size|Notes
---|---|---|---
`H + 1`|Hash|`256`|A SHA-256 hash of the imported module's contents.

## Function Definition Record
Describes a function defined in the current module. When generics are supported, a field will be added that lists generic parameters.

Offset|Bits|Name|Notes
---|---|---|---
`0`|`0`|Export|If set, indicates if this structure is visible to other modules.
`0`|`1`|Foreign|If set, indicates that the body of this function is defined elsewhere. Typically used when dealing with foreign function interface bindings.
`0`|`2..7`|Reserved|These bits must not be set.
`1`||Reserved|Reserved [integer](#integer-size) that will eventually be used for generic parameter information. Must be set to zero.
`1 + L`||Signature|An [integer](#integer-size) that indicates the [function's signature](#function-signatures).
`1 + 2L`||[Symbol](#symbols)
?||[Function Body](#function-body)

TODO: Move reserved generic parameter count to be with a Defined function body (before the index to the code block).

### Function Body
If the `Foreign` bit is not set, then the function body is simply a [length integer index](#integer-size) to a [code block](#code-block).

Otherwise, the function body describes a foreign function:

Name|Notes
---|---
Library|An [integer](#integer-size) index to an [identifier](#identifier-record) that specifies the name of the library that the function is defined in.
Entry Point Name|An [identifier](#identifier) that is the name of the function defined in the `Library`.

## Function Instantiation Record
Allows referring to function definitions and imports, while also allowing generic functions in the future.

TODO: Might rename to Instantiation Record since this structure might be reused by generic structs in the future.

Offset|Name|Notes
---|---|---
`0`|Template Index|An [integer](#integer-size) index used to refer to a function import or definition, where `0` refers to the first function import, and `x` refers to the first function definition, where `x` is the total number of function imports.
`L`|Reserved|A reserved [integer](#integer-size), must be set to zero.

# DEPRECATED BELOW

## Module Definitions
Contains the structs, functions, globals, etc. defined by the current module.

Name|Notes
---|---
Total Size|A [length integer](#integer-size) indicating the total size, in bytes, of all of the following module definition information. If zero, all following fields are omitted
Function Count|
[Function Definitions](#function-definition)|
Struct Count|
[Struct Definitions](#struct-definition)|
Global Count|
[Global Definitions]()|
Exception Class Count|Currently empty.
Exception Class Definitions|A [length integer](#integer-size), set to zero as SAILAR's exception handling mechanism is still being defined.
Annotation Class Count|Currently empty.
Annotation Class Definitions|A [length integer](#integer-size), set to zero as the semantics of annotations are still being decided.

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
`0`|Struct Index|A [length integer](#integer-size) used to refer to a struct import or definition, where `0` refers to the first struct import, and `x` refers to the first struct definition, where `x` is the total number of struct imports.
`L`|Reserved|A reserved [length integer](#integer-size), must be set to zero.

## Function Instantiations
Allows referring to function definitions and imports, while also allowing generic functions in the future.

Offset|Name|Notes
---|---|---
`0`|Total Size|
`L`|Count|
`2L`|

Offset|Name|Notes
---|---|---
`0`|Function Index|A [length integer](#integer-size) used to refer to a function import or definition, where `0` refers to the first function import, and `x` refers to the first function definition, where `x` is the total number of function imports.
`L`|Reserved|A reserved [length integer](#integer-size), must be set to zero.

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
`0`|Size|A [length integer](#integer-size) indicating the size of the following debugging information in bytes.
`L`|Debugging Information|The debugging information, [whose format is documented here](./debug-format.md).
