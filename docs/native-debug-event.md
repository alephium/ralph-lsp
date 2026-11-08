# Native Debug definition

Go-to-definition on `emit Debug(...)` opens the native event in `events.ral`.
`Debug` is a compiler intrinsic: its argument is an interpolated string, its
encoded message uses bytes, and release compilation removes it. It is not an
ordinary user-defined event. The generated abstract contract is for inspection only.
The strict dependency tree contains the empty abstract contract, while the soft tree
contains the event declaration. Offsets stay aligned. User source continues to
use the unmodified compiler parser, which rejects declarations of `event Debug`.
