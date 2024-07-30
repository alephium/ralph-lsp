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

package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.lsp.access.compiler.ast.node.{Node, NodeBuilder}
import org.alephium.ralph.{SourceIndex, Ast}

/** Ralph Syntax Tree (AST) */
sealed trait Tree {

  /** Every node must contain source information */
  def index: SourceIndex

}

object Tree {

  /**
   * Root node contains [[Statement]]s that are siblings
   * which can either be [[Import]] or [[Source]].
   */
  case class Root(
      statements: Seq[Statement],
      index: SourceIndex)
    extends Tree

  sealed trait Statement extends Tree

  /**
   * An import statement e.g. `import "std/nft_interface"`
   *
   * @param string The string content e.g: `"std/nft_interface"`
   * @param path   The file and folder paths
   * @param index  Index of the full import statement.
   */
  case class Import(
      string: StringLiteral,
      path: Option[ImportPath],
      index: SourceIndex)
    extends Statement

  case class ImportPath(
      folder: Name,
      file: Name,
      index: SourceIndex)
    extends Tree

  case class Source(
      ast: Ast.MultiContractDef,
      index: SourceIndex)
    extends Statement {
    // TODO: Move the following to a cache like Caffeine.

    /**
     * The root [[Node]] of the tree created from [[ast]].
     *
     * @note Lazily initialised as it can have concurrent access or no access at all.
     */
    lazy val rootNode: Node[Ast.MultiContractDef, Ast.Positioned] =
      NodeBuilder.buildRootNode(ast)

    def typeId(): Option[Ast.TypeId] =
      ast match {
        case ast: Ast.ContractWithState =>
          Some(ast.ident)

        case ast: Ast.Struct =>
          Some(ast.id)

        case ast: Ast.EnumDef[_] =>
          Some(ast.id)

        case _: Ast.ConstantVarDef[_] =>
          None
      }

  }

  sealed trait Literal extends Tree

  /**
   * @param value The string value with quotes.
   * @param name  The string value without quotes.
   * @param index [[SourceIndex]] of the string with quotes.
   */
  case class StringLiteral(
      value: String,
      name: Name,
      index: SourceIndex)
    extends Literal

  case class Name(
      value: String,
      index: SourceIndex)
    extends Tree

}
