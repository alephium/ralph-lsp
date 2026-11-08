# Native annotation definitions

Annotation names `@using`, `@inline` and `@std`, and their known property keys,
resolve to `annotations.ral`. That file represents annotation definitions as
interface signatures, not callable functions. Names and keys come from the
pinned compiler's annotation definitions. `using` combines the function and
interface keys; `std` combines interface `id` and contract `enabled`. This is
navigation metadata, not validation of which keys are legal at a given site.
The compiler still performs that validation. `assetsInContract` also supports
`enforced`, as documented next to its metadata signature.

Annotation navigation only searches that native file. An unknown annotation or
property never falls back to an unrelated variable or function of the same name.
Values inside annotation assignments retain ordinary expression navigation.
The annotation signatures are excluded from ordinary built-in inheritance
search, so `using()` does not resolve to an annotation declaration.
