package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node

object GoToFuncId {

  /** Given a [[Ast.FuncId]], jump to where this function is defined. */
  def goTo(funcIdNode: Node[Positioned],
           funcId: Ast.FuncId,
           source: Tree.Source): Option[Ast.FuncId] =
    funcIdNode
      .parent // take one step up to check the type of function call.
      .map(_.data)
      .collect {
        case callExpr: Ast.CallExpr[_] if callExpr.id == funcId =>
          // The user clicked on a local function. Take 'em there!
          goToLocalFunction(
            funcId = funcId,
            source = source
          )

        case callExpr: Ast.ContractCallExpr if callExpr.callId == funcId =>
          // TODO: The user clicked on a external function call. Take 'em there!
          None
      }
      .flatten

  /** Find the local function with the given function id ([[Ast.FuncId]]) */
  private def goToLocalFunction(funcId: Ast.FuncId,
                                source: Tree.Source): Option[Ast.FuncId] =
    funcId
      .sourceIndex
      .flatMap {
        sourceIndex =>
          source
            .scopeTable
            .nearestFunction(
              name = funcId.name,
              nearestToIndex = sourceIndex
            )
      }
      .map(_.typeDef)
}
