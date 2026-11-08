# Nested tuple types

A variable inferred as a tuple receives one inlay hint preserving its complete
shape, such as `: (Bool, (U256, Bool))`. Repeated leaf types remain in the display.
Compiler-generated tuple structs are expanded through compiled global-state metadata.
Destructuring selects each tuple element; multiple return values continue to be
handled independently.

Go-to-type-definition on a tuple variable returns the declarations of its leaf
types, with repeated declarations removed. Tuples do not have a synthetic named
declaration. The navigation results retain real source locations, separately
from the aggregate string used for the hint. Missing cached type information
continues to produce no result. Hints remain plain strings in the LSP response.
