# Native Debug definition

Go-to-definition on `emit Debug(...)` opens the native event in `events.ral`.
`Debug` is a compiler intrinsic: its argument is an interpolated string, its
encoded message uses bytes, and release compilation removes it. It is not an
ordinary user-defined event. The generated struct is inspection
metadata, not a callable function. Its persisted text is valid Ralph and is
parsed identically during initial dependency loading and subsequent rebuilds.
Navigation exposes this metadata only for `emit` targets. User source continues
to use the unmodified compiler parser, which rejects `event Debug` declarations.
