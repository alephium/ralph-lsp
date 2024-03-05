package org.alephium.ralph.lsp.access.compiler.ast.scope

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

  /** A function [[Ast.FuncId]] and it's scope. */
  case class Function(funcDef: Ast.FuncDef[_],
                      scope: SourceIndex) extends Scope {
    /**
     * The targeted AST, when the user clicks on a function call
     * will take the user to [[funcDef.id]].
     *
     * TODO: Show the entire definition of the function `fn my_func(a: A, b: B)`
     * to be highlighted but this information is not available via a single AST type.
     * */
    override def typeDef: Ast.FuncId =
      funcDef.id
  }
}
