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

import org.alephium.ralph
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

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

  /**
   * Represents a suggested argument.
   *
   * @param node                The node representing the argument.
   * @param isTemplateArgument  If true, this argument is suggested as a [[Completion.Property]],
   *                            otherwise as a [[Completion.Field]].
   */
  case class Argument private (
      node: SourceLocation.Node[Ast.Argument],
      isTemplateArgument: Boolean)
    extends Suggestion {

    override def toCompletion(): Seq[Completion] = {
      val typeSig =
        node.ast.signature.replaceAll("(:)", "$1 ")

      val suggestion =
        if (isTemplateArgument)
          Completion.Property(
            label = typeSig,
            insert = node.ast.ident.name,
            detail = ""
          )
        else
          Completion.Field(
            label = typeSig,
            insert = node.ast.ident.name,
            detail = ""
          )

      Seq(suggestion)
    }

  }

  /**
   * Represents a suggested variable definition.
   *
   * @param node The node representing the location where the variable was created.
   */
  case class VarDef(node: SourceLocation.Node[Ast.VarDef[_]]) extends Suggestion {

    override def toCompletion(): Seq[Completion.Variable] =
      node.ast.vars flatMap {
        case Ast.NamedVar(mutable, ident) =>
          val mutOrNot = if (mutable) s"${ralph.Keyword.mut.name} " else ""
          val typeSig  = s"$mutOrNot${ident.name}"

          val suggestion =
            Completion.Variable(
              label = typeSig,
              insert = ident.name,
              detail = ""
            )

          Some(suggestion)

        case Ast.AnonymousVar =>
          None
      }

  }

}
