# Function Template Record

A function template provides the [signature](./function_signature.md) and body of a [function](./function.md).

The function template system is made to help eventually add support for generic parameters.

## Structure

Without any generic parameters, and with a body defined in SAILAR code, a function signature record consists of:

- The [export information](../export_information.md) for the function
- A [variable width integer index] to a [function signature](./function_signature.md)
- A [variable width integer index] to the function's entry block, which is the code block that control flow is tranferred to when the function is called

TODO: Figure out how foreign impl/native functions are represented.

[variable width integer index]: ../values.md#variable-length-integers
