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
