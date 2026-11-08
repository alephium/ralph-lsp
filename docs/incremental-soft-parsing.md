# Incremental soft parsing

Client updates retain one previously evaluated soft parse for that file. The next
lazy soft parse uses `CompilerAccess.parseSoftIncremental`; it does not evaluate
old lazy parses solely to populate this cache or retain a chain of old states.
Strict parsing and compilation continue normally. The LSP server continues to
advertise full text synchronization; reuse is derived from the common UTF-16
prefix of the old and new document text.

A FastParse run-local cache contains complete template/function declarations
entirely before the first changed code unit. Declarations with missing or
unresolved syntax are excluded. The normal grammar parses the surrounding
context. When `BlockPartParser` reaches a cached declaration at its original
absolute offset, it reuses that parsed syntax and advances directly to its end.
This also reuses earlier functions when editing a later function in the same
contract. It does not reuse shifted suffixes or incomplete declarations.

Reused syntax is structurally copied with each original source index preserved.
This avoids sharing the AST's cached `Node` wrappers, whose parent links are
mutable, between old and new roots. There is no global cache or thread-local
state. Failure tracing runs the ordinary grammar to retain diagnostic behavior.

Without an evaluated previous tree or safe candidates, the full soft grammar
runs. Edits near the start of a file and edits within its first function may have
little or no reuse. This change does not claim that every edit becomes faster;
common-prefix discovery, candidate traversal and syntax copying still cost work.

Validation commands:

- `compiler-access/testOnly *IncrementalSoftParserSpec`
- `presentation-compiler/testOnly *SourceCodeIncrementalSpec *SourceCodeParseSpec *SourceCodePutOrRemoveSpec`

The parser tests compare complete ASTs, source reconstruction and offsets against
full parsing for insertions, deletions, malformed syntax, Unicode, CRLF and
repeated changes. They count actual skipped declarations, cover reuse inside a
single contract, and verify that old and new parent chains remain independent.
