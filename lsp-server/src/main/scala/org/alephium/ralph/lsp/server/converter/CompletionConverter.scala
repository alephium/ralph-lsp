// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.eclipse.lsp4j._

import java.util
import scala.jdk.CollectionConverters.SeqHasAsJava

/** Implements functions that transform internal code-completion types to LSP4J */
object CompletionConverter {

  /** Convert a sequence of code completion result to LSP4J type. */
  def toCompletionList(suggestions: Iterator[Suggestion]): CompletionList = {
    val items = new util.ArrayList[CompletionItem]()

    suggestions foreach {
      suggestion =>
        items addAll toCompletionItem(suggestion).asJava
    }

    new CompletionList(items)
  }

  /** Convert code completion result of type [[Suggestion]] to LSP4J type [[CompletionItem]]. */
  private def toCompletionItem(suggestion: Suggestion): Seq[CompletionItem] =
    suggestion.toCompletion() map {
      completion =>
        val item = new CompletionItem()
        item.setLabel(completion.label)
        item.setDetail(completion.detail)
        item.setInsertText(completion.insert)
        item.setKind(CompletionItemKind.valueOf(completion.productPrefix))
        item
    }

}
