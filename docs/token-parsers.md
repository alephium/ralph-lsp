# Static token parsers

`TokenParsers.parse(tokens, prefixCheck)` is a Scala 2 macro. The token expression
must be available at compile time, such as `Token.reserved.toSeq` or
`Seq(Token.True, Token.False)`. Empty collections, duplicate lexemes, and runtime
method arguments are rejected during compilation. The result retains the input
token subtype, including singleton types. Pass the Boolean positionally, for example
`TokenParsers.parse(Token.infix.toSeq, false)`; Scala 2 macro calls do not support
named arguments.

The `token-macros` project compiles the token model and macro implementation before
`compiler-access`. This keeps `Token` as the source of truth for lexemes and avoids
maintaining a second list of strings. Its package name is unchanged.

Expansion sorts by descending lexeme length and reverse lexical order, matching
`Token.compare`. Without prefix checks it emits FastParse's literal `StringIn`
trie followed by a direct string match returning token singletons. There is no
runtime token collection traversal, reflective lookup, or parser construction.
Generated parser entries, literal tries and singleton dispatch use `@noinline`
helpers so whole-program optimization does not duplicate them across the grammar
and combine them into oversized control-flow graphs.
With prefix checks it emits static guarded alternatives: for example a unary
`+` cannot consume the start of `++` or `+=`.

Reserved, infix, unary, Boolean, unit, template, inheritance, and whitespace
families use the macro. Runtime delimiter/stop lists in generic text and code
parsers remain dynamic because their values depend on the caller. Whitespace
expansion emits both LF and CRLF branches, selected using the runtime platform,
so a Linux-built JAR does not freeze `Token.Newline` to Linux behavior.

Run `compiler-access/test` for parser regressions. `TokenParsersSpec` covers every
reserved/infix token, reversed input order, overlapping operators, prefix checks,
typed singleton results, platform whitespace, invalid input and compile-time
rejections. Historical speedups in issues #605/#608 are not new benchmark results
for this implementation.
