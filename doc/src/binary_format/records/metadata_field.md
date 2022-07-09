# Metadata Record

The contents of metadata records contain information describing the module, such as its name or version. All metadata records
begin with an [identifier string] known as the field name, which indicates the meaning of the remaining bytes.

Field Name|Remaining Contents
-----------------|-
`id`             |An [module identifier] indicating the name and version of the module.
`main`           |A [variable width integer] index to a function instantiation indicating the entry point function of the module.

[identifier string]: ../values.md#strings
[module identifier]: ../values.md#module-identifiers
[variable width integer]: ../values.md#variable-length-integers
