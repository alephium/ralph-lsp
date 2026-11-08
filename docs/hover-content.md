# Repeated inherited hover signatures

Hover results can contain an inherited abstract declaration and its implementation. When those declarations render identical content, the hover provider now emits that content once. The first result and the existing order are retained.

Deduplication compares the displayed source text, not names alone. Different signatures, parameter types, documentation or other visible content remain available, including ambiguous declarations in incomplete source. Go-to-definition and reference results are unchanged.
