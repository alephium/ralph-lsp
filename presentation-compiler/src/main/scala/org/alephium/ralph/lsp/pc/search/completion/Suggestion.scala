// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.util.StringUtil
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

sealed trait Suggestion {

  /** Transform this suggest to a String based LSP format */
  def toCompletion(): Seq[Completion]

}

object Suggestion {

  /** A suggestion that points to a node within a tree */
  sealed trait NodeAPI extends Suggestion {

    def node: SourceLocation.NodeStrict[Ast.Positioned]

  }

  /** A suggestion available due to inheritance */
  sealed trait InheritedAPI extends NodeAPI

  /**
   * Provides created/named type and primitive type info.
   */
  sealed trait Type extends Suggestion

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

  object Keyword {

    def expression(keyword: ralph.Keyword): Suggestion.Keyword =
      new Keyword(
        label = keyword.name,
        insert = s"${keyword.name} ",
        detail = ""
      )

    def value(keyword: ralph.Keyword): Suggestion.Keyword =
      new Keyword(
        label = keyword.name,
        insert = keyword.name,
        detail = ""
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
   * Represents a suggestion for an annotation field.
   *
   * @param label The label or display text for the annotation field suggestion.
   * @param insert The text to insert when the annotation field suggestion is selected.
   * @param detail Additional details about the annotation field suggestion.
   */
  case class AnnotationField(
      label: String,
      insert: String,
      detail: String)
    extends Suggestion {

    override def toCompletion(): Seq[Completion.Field] =
      Seq(
        Completion.Field(
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
      node: SourceLocation.NodeStrict[Ast.Argument],
      isTemplateArgument: Boolean)
    extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion] = {
      val typeSignature =
        StringUtil.spaceBetweenCommaAndSemicolon(node.ast.tpe.signature)

      val label =
        s"${node.ast.ident.name}: $typeSignature"

      val details =
        if (node.ast.isMutable)
          s"${ralph.Keyword.mut.name} ${node.ast.ident.name}: $typeSignature"
        else
          ""

      val suggestion =
        if (isTemplateArgument)
          Completion.Property(
            label = label,
            insert = node.ast.ident.name,
            detail = details
          )
        else
          Completion.Field(
            label = label,
            insert = node.ast.ident.name,
            detail = details
          )

      Seq(suggestion)
    }

  }

  /**
   * Represents a suggested variable definition.
   *
   * @param node The node representing the location where the variable was created.
   */
  case class VarDef(node: SourceLocation.NodeStrict[Ast.VarDef[_]]) extends NodeAPI {

    override def toCompletion(): Seq[Completion.Variable] =
      node.ast.vars flatMap {
        case Ast.NamedVar(mutable, ident) =>
          val details =
            if (mutable)
              s"${ralph.Keyword.mut.name} ${ident.name}"
            else
              ""

          val suggestion =
            Completion.Variable(
              label = ident.name,
              insert = ident.name,
              detail = details
            )

          Some(suggestion)

        case _: Ast.AnonymousVar =>
          None
      }

  }

  case class FuncDef(
      node: SourceLocation.NodeStrict[Ast.FuncDef[_]],
      isBuiltIn: Boolean)
    extends Suggestion.InheritedAPI {

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
            .map {
              tpe =>
                StringUtil.spaceBetweenCommaAndSemicolon(tpe.signature)
            }
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

  case class EventDef(node: SourceLocation.NodeStrict[Ast.EventDef]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Event] =
      Seq(
        Completion.Event(
          label = node.ast.id.name,
          insert = node.ast.id.name,
          detail = StringUtil.spaceBetweenCommaAndSemicolon(node.ast.signature)
        )
      )

  }

  case class EnumDef(node: SourceLocation.NodeStrict[Ast.EnumDef[_]]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Enum] =
      Seq(
        Completion.Enum(
          label = node.ast.id.name,
          insert = node.ast.id.name,
          detail = ""
        )
      )

  }

  case class ConstantVarDef(node: SourceLocation.NodeStrict[Ast.ConstantVarDef[_]]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Constant] =
      Seq(
        Completion.Constant(
          label = node.ast.ident.name,
          insert = node.ast.ident.name,
          detail = ""
        )
      )

  }

  case class MapDef(node: SourceLocation.NodeStrict[Ast.MapDef]) extends Suggestion.InheritedAPI {

    override def toCompletion(): Seq[Completion.Property] = {
      val label =
        s"""${node.ast.ident.name}: ${StringUtil.spaceBetweenCommaAndSemicolon(node.ast.tpe.signature)}"""

      val property =
        Completion.Property(
          label = label,
          insert = node.ast.ident.name,
          detail = ""
        )

      Seq(property)
    }

  }

  /**
   * Represents suggestions for all enum fields defined within the given enum definition.
   *
   * @param node The node containing the enum definition.
   */
  case class EnumFields(node: SourceLocation.NodeStrict[Ast.EnumDef[_]]) extends Suggestion.NodeAPI {

    override def toCompletion(): Seq[Completion.EnumMember] =
      node.ast.fields map {
        field =>
          Completion.EnumMember(
            label = field.name,
            insert = field.name,
            detail = ""
          )
      }

  }

  /**
   * Represents types that are created in the workspace.
   *
   * @param node The node containing the type identifier.
   */
  case class CreatedType(node: SourceLocation.NodeStrict[Ast.TypeId]) extends Type {

    override def toCompletion(): Seq[Completion.Class] =
      Seq(
        Completion.Class(
          label = node.ast.name,
          insert = node.ast.name,
          detail = ""
        )
      )

  }

  /**
   * Represents a Ralph primitive type.
   *
   * @param tpe The primitive type.
   */
  case class PrimitiveType(tpe: ralph.Type) extends Type {

    override def toCompletion(): Seq[Completion.Class] = {
      val signature = tpe.signature

      Seq(
        Completion.Class(
          label = signature,
          insert = signature,
          detail = ""
        )
      )
    }

  }

}
