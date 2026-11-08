# Definition lookup while typing asset approvals

Before argument parentheses are typed, a form such as `blah{user}` can look like an incomplete struct constructor. For a lowercase name with nonempty braces and no explicit struct field separator, definition lookup now first tries the callable interpretation. If a callable is in scope, it gives the same kind of result as `blah{user}()`; otherwise it falls back to the original struct lookup.

Explicit struct fields (`blah{user: value}`), empty braces and uppercase struct names retain their existing interpretation. This is an editor recovery heuristic for incomplete code, not a change to Ralph syntax or compiler validation. The temporary call node uses a deep copy so it does not mutate parent links in the parsed syntax tree.
