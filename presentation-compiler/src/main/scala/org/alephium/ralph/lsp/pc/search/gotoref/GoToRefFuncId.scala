// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{ImplementingChildrenResult, WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import scala.collection.immutable.ArraySeq

private object GoToRefFuncId extends StrictImplicitLogging {

  def goTo(
      definition: Node[Ast.FuncId, Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToRefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    definition.parent match {
      case Some(parent) =>
        parent match {
          case Node(funcDef: Ast.FuncDef[_], _) =>
            val result =
              goToFunctionUsage(
                funcId = funcDef.id,
                sourceCode = sourceCode,
                workspace = workspace
              )

            IncludeDeclaration.add(
              definitionAST = funcDef.id,
              definitionSource = sourceCode,
              result = result,
              isIncludeDeclaration = settings.includeDeclaration
            )

          case Node(ast, _) =>
            logger.error(s"No GoToRef implementation for '${ast.getClass.getSimpleName}'")
            Iterator.empty
        }

      case None =>
        logger.error(s"Parent node not found for AST '${definition.data.getClass.getSimpleName}' at source index '${definition.data.sourceIndex}'")
        Iterator.empty
    }

  /**
   * Navigate to all function usages within the source code for the specified [[Ast.FuncId]].
   *
   * @param funcId     The [[Ast.FuncId]] of the function to locate.
   * @param sourceCode The source tree to search.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over all searched function usages.
   */
  private def goToFunctionUsage(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    if (funcId.isBuiltIn) {
      val allTrees =
        WorkspaceSearcher.collectAllTrees(workspace)

      goToDirectCallFunctionUsage(
        funcId = funcId,
        children = allTrees
      )
    } else {
      val children =
        WorkspaceSearcher.collectImplementingChildren(
          sourceCode = sourceCode,
          workspace = workspace
        )

      // Direct calls can only occur within the scope of inheritance.
      val directCallUsages =
        goToDirectCallFunctionUsage(
          funcId = funcId,
          children = children.childTrees
        )

      // Contract call can occur anywhere where an instance of the Contract can be created.
      val contractCallUsages =
        goToContractCallFunctionUsage(
          funcId = funcId,
          children = children
        )

      directCallUsages ++ contractCallUsages
    }

  /**
   * Navigates to all direct function call usages for the specified [[Ast.FuncId]].
   *
   * @param funcId   The [[Ast.FuncId]] of the function to locate usages for.
   * @param children The sources to search within.
   * @return An iterator over all found function call usages.
   */
  private def goToDirectCallFunctionUsage(
      funcId: Ast.FuncId,
      children: ArraySeq[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    children
      .iterator
      .flatMap {
        sourceCode =>
          goToFunctionUsage(
            funcId = funcId,
            sourceCode = sourceCode
          )
      }

  /**
   * Navigate to all local function usage where the given function definition [[Ast.FuncDef]]
   * is invoked.
   *
   * @param funcId     The [[Ast.FuncId]] of the [[Ast.FuncDef]] to locate calls for.
   * @param sourceCode The source tree to search within.
   * @return An iterator containing all the local function calls.
   */
  private def goToFunctionUsage(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(exp: Ast.CallExpr[_], _) if exp.id == funcId =>
          SourceLocation.NodeStrict(exp.id, sourceCode)

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          SourceLocation.NodeStrict(funcCall.id, sourceCode)
      }

  /**
   * Navigates to all contract call usages for the specified [[Ast.FuncId]].
   *
   * @param funcId   The [[Ast.FuncId]] of the function to locate contract call usages for.
   * @param children The result containing the child trees and all trees in scope within the current workspace.
   * @return An iterator over all found contract call usages.
   */
  private def goToContractCallFunctionUsage(
      funcId: Ast.FuncId,
      children: ImplementingChildrenResult): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    children
      .allTrees // traverse all trees to find contract call usages, eg: `myContract.function()` or `MyContract().function()`
      .iterator
      .flatMap {
        code =>
          code.tree.rootNode.walkDown.collect {
            case Node(call: Ast.ContractCallBase, _) if call.callId == funcId =>
              // function ID matches, but does it also match the type ID?
              call
                .obj
                .getCachedType()
                .map(_.flatMap(AstExtra.getTypeId)) // fetch the type ID of this function call
                .iterator
                .flatMap {
                  thisCallTypes =>
                    children
                      .childTrees // at least one of the child trees should match this call's object type
                      .flatMap {
                        childCode =>
                          if (childCode.tree.typeId() exists thisCallTypes.contains)
                            Some(SourceLocation.NodeStrict(call.callId, code)) // Matched! Both the `funcId` and `typeId` are a match.
                          else
                            None
                      }
                }
          }
      }
      .flatten

}
