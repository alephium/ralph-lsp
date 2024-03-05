package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.eclipse.lsp4j._

import java.util
import scala.collection.immutable.ArraySeq

/** Implements functions that transform internal code-completion types to LSP4J */
object CompletionConverter {

  def toCompletionList(suggestions: ArraySeq[Suggestion]): CompletionList = {
    val items = new util.ArrayList[CompletionItem]()

    suggestions foreach {
      suggestion =>
        val item = new CompletionItem()
        item.setLabel(suggestion.label)
        item.setDetail(suggestion.detail)
        item.setDocumentation(suggestion.documentation)
        item.setInsertText(suggestion.insert)
        item.setKind(CompletionItemKind.valueOf(suggestion.productPrefix))
        items.add(item)
    }

    new CompletionList(items)
  }

}
