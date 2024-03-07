package org.alephium.ralph.lsp.pc.search.scope

import org.alephium.ralph.{Ast, SourceIndex}

sealed trait Scope {
  /** The targeted type definition */
  def typeDef: Ast.Positioned

  /** The scope of the targeted type definition */
  def scope: SourceIndex
}

object Scope {

  /** An argument [[Ast.Argument]] and it's scope */
  case class Argument(typeDef: Ast.Argument,
                      scope: SourceIndex) extends Scope
}
