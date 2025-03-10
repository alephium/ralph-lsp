// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.Keyword

object ContractBodyCompleter {

  def suggest(): Iterator[Suggestion.Keyword] =
    suggestKeywords()

  /**
   * Suggests keywords relevant to a Contract's body.
   */
  private def suggestKeywords(): Iterator[Suggestion.Keyword] =
    Iterator(
      Suggestion.Keyword.expression(Keyword.pub),
      Suggestion.Keyword.expression(Keyword.fn),
      Suggestion.Keyword.expression(Keyword.`enum`),
      Suggestion.Keyword.expression(Keyword.event),
      Suggestion.Keyword.expression(Keyword.mapping),
      Suggestion.Keyword.expression(Keyword.struct),
      Suggestion.Keyword.expression(Keyword.`extends`),
      Suggestion.Keyword.expression(Keyword.implements)
    )

}
