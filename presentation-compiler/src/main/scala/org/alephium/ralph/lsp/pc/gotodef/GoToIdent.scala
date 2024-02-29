package org.alephium.ralph.lsp.pc.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Node

object GoToIdent {

  /** Given a [[Node]] of type ident [[Ast.Ident]] provide go-to definition */
  def goTo(identNode: Node[Positioned],
           ident: Ast.Ident): Option[Ast.Argument] =
    identNode
      .parent // take one step up to check the type of ident node.
      .map(_.data)
      .collect {
        case variable: Ast.Variable[_] if variable.id == ident => // Is it a variable?
          // The user clicked on a variable. Take 'em there!
          goToVariable(
            variableNode = identNode,
            variable = variable
          )
      }
      .flatten

  private def goToVariable(variableNode: Node[Positioned],
                           variable: Ast.Variable[_]): Option[Ast.Argument] =
    variableNode
      .walkUpDown // walk up from the clicked variable node to find the nearest argument.
      .collectFirst {
        case positioned @ Ast.Argument(ident: Ast.Ident, _, _, _) if ident == variable.id =>
          positioned

        // TODO: Add support other node types such as `NamedVar`.
      }
}
