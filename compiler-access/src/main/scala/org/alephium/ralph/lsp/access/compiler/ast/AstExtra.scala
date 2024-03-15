package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.protocol.vm.StatelessContext
import org.alephium.ralph.Ast

object AstExtra {

  /**
   * Checks if the variable definition [[Ast.VarDef]]
   * contains a named variable with the given identifier [[Ast.Ident]].
   *
   * @param varDef The variable definition to check.
   * @param ident  The identifier of the variable to search for.
   * @return True if the variable definition contains a named variable with the given identifier, false otherwise.
   */
  def containsNamedVar[A <: StatelessContext](varDef: Ast.VarDef[A],
                                              ident: Ast.Ident): Boolean =
    varDef
      .vars
      .exists {
        case varType: Ast.NamedVar =>
          varType.ident == ident

        case _ =>
          false
      }

}
