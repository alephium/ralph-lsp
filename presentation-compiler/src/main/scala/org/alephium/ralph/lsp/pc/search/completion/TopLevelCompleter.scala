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

  private def suggestContract(): Suggestion.Keyword =
    Suggestion.Keyword(
      label = "Contract",
      insert = "Contract ",
      detail = "Similar to classes in object-oriented languages"
    )

  private def suggestAbstractContract(): Suggestion.Keyword =
    Suggestion.Keyword(
      label = "Abstract Contract",
      insert = "Abstract Contract ",
      detail = "Similar to abstract classes in object-oriented languages"
    )

  private def suggestInterface(): Suggestion.Keyword =
    Suggestion.Keyword(
      label = "Interface",
      insert = "Interface ",
      detail = "Similar to interfaces in object-oriented languages"
    )

  private def suggestTxScript(): Suggestion.Keyword =
    Suggestion.Keyword(
      label = "TxScript",
      insert = "TxScript ",
      detail = "Code to interact with contracts on the blockchain"
    )

}
