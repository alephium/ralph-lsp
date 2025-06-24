// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.{Ast, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.ast.node.NodeBuilder
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.utils.Node

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
      ast: Ast.GlobalDefinition,
      index: SourceIndex)
    extends Statement {
    // TODO: Move the following to a cache like Caffeine.

    /**
     * The root [[Node]] of the tree created from [[ast]].
     *
     * @note Lazily initialised as it can have concurrent access or no access at all.
     */
    lazy val rootNode: Node[Ast.GlobalDefinition, Ast.Positioned] =
      NodeBuilder.buildRootNode(ast)

    def typeId(): Option[Ast.TypeId] =
      AstExtra.getTypeId(ast)

    /**
     * find the node closest to this source-index
     */
    def closest(index: Int): Option[Node[Ast.Positioned, Ast.Positioned]] =
      rootNode.findLast(_.sourceIndex.exists(_ contains index))

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
