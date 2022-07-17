# Function Record

A function record is an instantiation of a [function template], supplying all generic arguments if there are any.

Indices to function records are used in many places, such as to specify the target of a `call` instruction.

Without any generic parameters, function records are simply represented as a
[variable width integer index](../values.md#variable-length-integers) to a [function template]

[function template]: ./function_template.md
