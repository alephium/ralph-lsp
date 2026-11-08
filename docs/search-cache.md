# Incremental search cache

Each `WorkspaceSearchCache` describes one immutable workspace snapshot. A new
snapshot reuses unchanged files' lazy tree and import extraction by URI and parsed
state equality. Changed files receive fresh entries; deleted or inaccessible files
are absent. Parser-error states continue to use the soft parser, as in uncached
search. Constructing a cache does not force soft parsing.

Dependency scopes are shared while the compiled build remains equal. An unchanged
set of local imports reuses the imported/inherited/primitive tree array. Changing
imports recomputes that scope; changing the build creates a fresh dependency cache.
Built-in trees excluding primitives are extracted once per shared build cache.
Import membership uses a set rather than repeatedly scanning all imports.

`SearchCache` retains at most `max(1, maxWorkspaces)` recent workspace roots strongly,
in access order, alongside the existing weak snapshot lookup. This preserves reuse
between edits even when a collection runs between requests. Each retained root
holds only its latest accessed snapshot, and successor caches do not retain their
predecessor. Only the latest import-set scope is retained by the shared dependency
cache. Eviction or an older concurrent request can reduce reuse but cannot change
results: each snapshot retains its own lazy result, and the shared import lookup is
synchronized.

The first request still parses previously unseen files and dependencies. Each edit
still scans the current file list and assembles a result array; this change avoids
repeating unchanged-file extraction and unchanged dependency-scope construction.
It does not claim constant-time workspace updates or background indexing.

`SearchCacheSpec` checks entry/tree identity reuse and compares results against
`WorkspaceSearcher` after edits, deletion, unparsed/error transitions, import changes,
dependency replacement, and workspace-root eviction. These are deterministic work
reuse checks, not timing-sensitive benchmarks.
