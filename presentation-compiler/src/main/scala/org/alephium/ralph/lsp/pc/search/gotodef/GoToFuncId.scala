package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node

import scala.collection.immutable.ArraySeq

private object GoToFuncId {

  /**
   * Navigate to the definition of a function for the given [[Ast.FuncId]].
   *
   * @param funcIdNode The node representing the [[Ast.FuncId]] in the AST.
   * @param funcId     The [[Ast.FuncId]] of the function to find the definition for.
   * @param source     The source tree to search within.
   * @return An array sequence containing the positioned ASTs of the searched function.
   */
  def goTo(funcIdNode: Node[Positioned], funcId: Ast.FuncId, source: Tree.Source): ArraySeq[Ast.Positioned] =
    funcIdNode.parent // take one step up to check the type of function call.
      .map(_.data)
      .to(ArraySeq)
      .collect {
        case callExpr: Ast.CallExpr[_] if callExpr.id == funcId =>
          // The user clicked on a local function. Take 'em there!
          goToLocalFunction(
            funcId = funcId,
            source = source
          )

        case funcCall: Ast.FuncCall[_] if funcCall.id == funcId =>
          goToLocalFunction(
            funcId = funcCall.id,
            source = source
          )

        case funcDef: Ast.FuncDef[_] if funcDef.id == funcId =>
          goToFunctionCalls(
            funcId = funcDef.id,
            source = source
          )

        case callExpr: Ast.ContractCallExpr if callExpr.callId == funcId =>
          // TODO: The user clicked on a external function call. Take 'em there!
          ArraySeq.empty
      }
      .flatten

  /**
   * Navigate to the local function within the source code for the given [[Ast.FuncId]].
   *
   * @param funcId The [[Ast.FuncId]] of the local function to locate.
   * @param source The source tree to search within.
   * @return An array sequence containing all the local function definitions.
   */
  private def goToLocalFunction(funcId: Ast.FuncId, source: Tree.Source): ArraySeq[Ast.FuncId] =
    // TODO: Improve selection by checking function argument count and types.
    source.ast match {
      case Left(ast) =>
        ast.funcs
          .filter(_.id == funcId)
          .map(_.id)
          .to(ArraySeq)

      case Right(_) =>
        ArraySeq.empty
    }

  /**
   * Navigate to all local function calls where the given function definition [[Ast.FuncDef]]
   * is invoked.
   *
   * @param funcId The [[Ast.FuncId]] of the [[Ast.FuncDef]] to locate calls for.
   * @param source The source tree to search within.
   * @return An array sequence containing all the local function calls.
   */
  private def goToFunctionCalls(funcId: Ast.FuncId, source: Tree.Source): ArraySeq[Ast.Positioned] =
    source.rootNode.walkDown
      .collect {
        case Node(exp: Ast.CallExpr[_], _) if exp.id == funcId =>
          exp

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          funcCall
      }
      .to(ArraySeq)
}
