package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.{Node, Tree}

object GoToIdent {

  /** Given a [[Node]] of type ident [[Ast.Ident]] provide go-to definition */
  def goTo(identNode: Node[Positioned],
           ident: Ast.Ident,
           source: Tree.Source): Option[Ast.Argument] =
    identNode
      .parent // take one step up to check the type of ident node.
      .map(_.data)
      .collect {
        case variable: Ast.Variable[_] if variable.id == ident => // Is it a variable?
          // The user clicked on a variable. Take 'em there!
          goToVariable(
            variableNode = identNode,
            variable = variable,
            source = source
          )
      }
      .flatten

  private def goToVariable(variableNode: Node[Positioned],
                           variable: Ast.Variable[_],
                           source: Tree.Source): Option[Ast.Argument] =
    variable
      .sourceIndex
      .flatMap {
        variableIndex =>
          source
            .scopeTable
            .nearestArgument(
              name = variable.id.name,
              nearestToIndex = variableIndex
            )
      }
      .map(_.typeDef)
}
