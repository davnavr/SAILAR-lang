# Function Signature Record

A function signature specifies the return types and parameter types of a function, and consist of the following:

- A [variable width integer] indicating the number of return types
- A [variable width integer] indicating the number of parameters
- A sequence of [variable width integer indices](../values.md#variable-length-integers) to type signatures containing the return types and parameter types in that order

[variable width integer]: ../values.md#variable-length-integers
