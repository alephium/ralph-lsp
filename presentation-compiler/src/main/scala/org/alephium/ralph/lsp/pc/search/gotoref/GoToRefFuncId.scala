// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, ImplementingChildrenResult, WorkspaceSearcher}

import scala.collection.immutable.ArraySeq

object GoToRefFuncId extends StrictImplicitLogging {

  def goTo(
      definition: Node[Ast.FuncId, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware,
      isIncludeDeclaration: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
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
              isIncludeDeclaration = isIncludeDeclaration
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
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] =
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
      children: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.Positioned]] =
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
      sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.Positioned]] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case Node(exp: Ast.CallExpr[_], _) if exp.id == funcId =>
          SourceLocation.Node(exp.id, sourceCode)

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          SourceLocation.Node(funcCall.id, sourceCode)
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
      children: ImplementingChildrenResult): Iterator[SourceLocation.Node[Ast.Positioned]] =
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
                            Some(SourceLocation.Node(call.callId, code)) // Matched! Both the `funcId` and `typeId` are a match.
                          else
                            None
                      }
                }
          }
      }
      .flatten

}
