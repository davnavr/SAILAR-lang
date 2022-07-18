# Function Template Record

A function template provides the [signature](./function_signature.md) and body of a [function](./function.md).

The function template system is made to help eventually add support for generic parameters.

## Structure

In its simplest form, without any generic parameters, a function template record consists of:

- The [export information](../export_information.md) for the function
- A [variable width integer index] to a [function signature](./function_signature.md)
- A [variable width integer index] to the function's entry block, which is the code block that control flow is tranferred to when the function is called

[variable width integer index]: ../values.md#variable-length-integers
