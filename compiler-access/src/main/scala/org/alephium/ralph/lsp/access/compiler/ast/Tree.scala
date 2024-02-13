package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.Ast

/** Ralph Syntax Tree (AST) */
sealed trait Tree {

  /** Every node must contain source information */
  def index: SourceIndex
}

object Tree {
  /**
   * Root node contains [[Statement]]s that are siblings
   * which can either be [[Import]] or [[Source]].
   * */
  case class Root(statements: Seq[Statement],
                  index: SourceIndex) extends Tree

  sealed trait Statement extends Tree

  /**
   * An import statement e.g. `import "std/nft_interface"`
   *
   * @param string The string content e.g: `"std/nft_interface"`
   * @param path   The file and folder paths
   * @param index  Index of the full import statement.
   */
  case class Import(string: StringLiteral,
                    path: Option[Path],
                    index: SourceIndex) extends Statement

  case class Path(folder: Name,
                  file: Name,
                  index: SourceIndex) extends Tree

  case class Source(ast: Ast.ContractWithState,
                    index: SourceIndex) extends Statement

  sealed trait Literal extends Tree

  case class StringLiteral(name: Name,
                           index: SourceIndex) extends Literal

  case class Name(value: String,
                  index: SourceIndex) extends Tree
}
