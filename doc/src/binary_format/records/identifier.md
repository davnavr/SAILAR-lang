# Identifier Record

An identifier record simply contains the bytes that make up a UTF-8 string, and is usually used for identifiers that are expected
to be used throughout the module, such as the name of a referenced native library. Note that, when not in an [array record] the
byte length of the record contents indicates the length of the string. When contained in an [array record], a
[variable width integer](../values.md#variable-length-integers) indicating the length is prepended before the string contents,
effectively making the record a [byte vector containing a string](../values.md#strings).

Note that all identifier strings cannot contain any `NUL` (`0x00`) bytes. This is to allow compatibility of native code formats,
which typically use `null` terminated strings for symbol names.

[array record]: array.md
