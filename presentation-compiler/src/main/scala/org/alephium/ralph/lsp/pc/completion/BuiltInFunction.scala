package org.alephium.ralph.lsp.pc.completion

case class BuiltInFunction (
    name: String,
    category: String,
    signature: String,
    doc: String,
    params: Seq[String],
    returns: String
) {
    val toSuggestion: Suggestion.Function = Suggestion.Function(
        label = name,
        insert = name,
        detail = signature,
        documentation = doc
    )
}
