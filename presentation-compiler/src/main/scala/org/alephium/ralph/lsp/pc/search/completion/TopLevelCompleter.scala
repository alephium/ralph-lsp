// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.Keyword

import scala.collection.immutable.ArraySeq

object TopLevelCompleter {

  def suggest(): ArraySeq[Suggestion.Keyword] =
    ArraySeq(
      suggestImport(),
      suggestContract(),
      suggestAbstractContract(),
      suggestInterface(),
      suggestTxScript()
    )

  private def suggestImport(): Suggestion.Keyword =
    Suggestion.Keyword(
      label = "import",
      insert = """import """"",
      detail = "Import a dependency file to the project"
    )

  private def suggestContract(): Suggestion.Keyword = {
    val label = Keyword.Contract.name
    Suggestion.Keyword(
      label = label,
      insert = s"$label ",
      detail = "Similar to classes in object-oriented languages"
    )
  }

  private def suggestAbstractContract(): Suggestion.Keyword = {
    val label = s"${Keyword.Abstract.name} ${Keyword.Contract.name}"
    Suggestion.Keyword(
      label = label,
      insert = s"$label ",
      detail = "Similar to abstract classes in object-oriented languages"
    )
  }

  private def suggestInterface(): Suggestion.Keyword = {
    val label = Keyword.Interface.name
    Suggestion.Keyword(
      label = label,
      insert = s"$label ",
      detail = "Similar to interfaces in object-oriented languages"
    )
  }

  private def suggestTxScript(): Suggestion.Keyword = {
    val label = Keyword.TxScript.name
    Suggestion.Keyword(
      label = label,
      insert = s"$label ",
      detail = "Code to interact with contracts on the blockchain"
    )
  }

}
