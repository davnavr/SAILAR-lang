# Array Record

Array records are a special type of record that contain other records, avoiding repetition of byte lengths and record type tag
bytes for records that are of the same type. Note that array records cannot contain array records. Array records consist of a
byte tag indicating the kind of records contained by the array followed by a [vector](../values.md#vectors) containing the
records.
