// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
