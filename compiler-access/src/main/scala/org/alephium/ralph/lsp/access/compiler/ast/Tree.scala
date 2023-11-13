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

  case class Import(pkg: StringLiteral,
                    index: SourceIndex) extends Statement

  case class Source(ast: Ast.ContractWithState,
                    index: SourceIndex) extends Statement

  sealed trait Literal extends Tree

  case class StringLiteral(value: String,
                           index: SourceIndex) extends Literal
}
