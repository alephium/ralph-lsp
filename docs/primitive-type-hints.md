# Primitive names and user types

Inlay hints and go-to-type-definition distinguish compiler primitive types from
user-defined types, even when their names match. For example, inside a contract
named `ByteVec`, a byte literal receives one `ByteVec` hint whose type metadata
points to the built-in primitive. A value created with the contract constructor
points to the user contract instead. Separate variables still receive separate
hints; this is semantic source selection, not global label deduplication.
