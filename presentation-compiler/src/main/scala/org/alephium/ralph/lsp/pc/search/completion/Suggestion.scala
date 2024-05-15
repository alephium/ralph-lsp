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
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

sealed trait Suggestion {

  /** Transform this suggest to a String based LSP format */
  def toCompletion(): Seq[Completion]

}

object Suggestion {

  /** A suggestion that points to a node within a tree */
  sealed trait NodeAPI extends Suggestion {

    def node: SourceLocation.Node[Ast.Positioned]

  }

  /** A suggestion available due to inheritance */
  sealed trait InheritedAPI extends NodeAPI

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
    extends Suggestion.InheritedAPI {

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
  case class VarDef(node: SourceLocation.Node[Ast.VarDef[_]]) extends NodeAPI {

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

  case class FuncDef(node: SourceLocation.Node[Ast.FuncDef[_]]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion] = {
      val paramTypes =
        node
          .ast
          .args
          .map {
            argument =>
              s"""${argument.ident.name}: ${argument.tpe.signature}"""
          }
          .mkString("(", ", ", ")")

      val isBuiltIn =
        DependencyID.BuiltIn contains node.parsed.fileURI

      val suffix =
        if (isBuiltIn)
          "!"
        else
          ""

      val returnTypes =
        if (node.ast.rtypes.size == 1)
          node
            .ast
            .rtypes
            .head
            .signature
        else
          node
            .ast
            .rtypes
            .map(_.signature.replaceAll("(,)", "$1 "))
            .mkString("(", ", ", ")")

      val label =
        s"${node.ast.id.name}$suffix$paramTypes -> $returnTypes"

      val insert =
        s"${node.ast.id.name}$suffix()"

      val suggestion =
        if (isBuiltIn)
          Completion.Function(
            label = label,
            insert = insert,
            detail = ""
          )
        else
          Completion.Method(
            label = label,
            insert = insert,
            detail = ""
          )

      Seq(suggestion)
    }

  }

  case class EventDef(node: SourceLocation.Node[Ast.EventDef]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Event] =
      Seq(
        Completion.Event(
          label = node.ast.id.name,
          insert = node.ast.id.name,
          detail = node.ast.signature.replaceAll("([:,])", "$1 ")
        )
      )

  }

  case class EnumDef(node: SourceLocation.Node[Ast.EnumDef[_]]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Enum] =
      Seq(
        Completion.Enum(
          label = node.ast.id.name,
          insert = node.ast.id.name,
          detail = ""
        )
      )

  }

  case class ConstantVarDef(node: SourceLocation.Node[Ast.ConstantVarDef[_]]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Constant] =
      Seq(
        Completion.Constant(
          label = node.ast.ident.name,
          insert = node.ast.ident.name,
          detail = ""
        )
      )

  }

}
