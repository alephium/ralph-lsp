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

sealed trait Suggestion {

  /** Transform this suggest to a String based LSP format */
  def toCompletion(): Seq[Completion]

}

object Suggestion {

  /**
   * Represents a suggestion for a file name.
   *
   * @param name The name of the file.
   */
  case class File(name: String) extends Suggestion {

    override def toCompletion(): Seq[Completion.File] =
      Seq(
        Completion.File(
          label = name,
          insert = name,
          detail = ""
        )
      )

  }

  /**
   * Represents a suggestion for a keyword.
   *
   * @param label The label or display text for the keyword suggestion.
   * @param insert The text to insert when the keyword suggestion is selected.
   * @param detail Additional details about the keyword suggestion.
   */
  case class Keyword(
      label: String,
      insert: String,
      detail: String)
    extends Suggestion {

    override def toCompletion(): Seq[Completion.Keyword] =
      Seq(
        Completion.Keyword(
          label = label,
          insert = insert,
          detail = detail
        )
      )

  }

}
