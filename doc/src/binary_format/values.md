# Values

## Variable Length Integers

To reduce the size of modules, almost all integers in a SAILAR module are stored in a variable-length format.

TODO: Insert description of format here.

## Vectors

A list of things in a SAILAR module is usually encoded as a [variable width integer](#variable-length-integers) indicating the
length followed by the elements of the vector.

## Strings

The majority of strings in a SAILAR module are stored as a [byte vector](#vectors) containing valid UTF-8.

## Module Identifiers

Indicates the name and version of a SAILAR module. Consists of a [name string](#strings) followed by a [vector](#vectors) of
[variable width integers](#variable-length-integers) indicating the module version.
