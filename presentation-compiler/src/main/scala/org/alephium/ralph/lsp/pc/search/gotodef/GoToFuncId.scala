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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.AstExtra
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, ImplementingChildrenResult, WorkspaceSearcher}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

import scala.collection.immutable.ArraySeq

private[search] object GoToFuncId extends StrictImplicitLogging {

  /**
   * Navigate to the definition of a function for the given [[Ast.FuncId]].
   *
   * @param funcIdNode The node representing the [[Ast.FuncId]] in the AST.
   * @param sourceCode The source-tree and its parsed source-code state, where this search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence containing the positioned ASTs of the searched function.
   */
  def goTo(
      funcIdNode: Node[Ast.FuncId, Ast.Positioned],
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
    funcIdNode.parent match { // take one step up to check the type of function call.
      case Some(parent) =>
        parent match {
          case Node(callExpr: Ast.CallExpr[_], _) if callExpr.id == funcIdNode.data =>
            // The user clicked on a local function. Take 'em there!
            goToFunction(
              funcId = callExpr.id,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcIdNode.data =>
            goToFunction(
              funcId = funcCall.id,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(funcDef: Ast.FuncDef[_], _) if funcDef.id == funcIdNode.data =>
            goToFunctionUsage(
              funcId = funcIdNode.data,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(call: Ast.ContractCallBase, _) if call.callId == funcIdNode.data =>
            goToFunctionImplementation(
              functionId = funcIdNode.data,
              typeExpr = call.obj,
              workspace = workspace
            )

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
    }

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  def goToNearestFuncDef(childNode: Node[Ast.Positioned, Ast.Positioned]): Option[Node[Ast.FuncDef[_], Ast.Positioned]] =
    childNode.data match {
      case function: Ast.FuncDef[_] =>
        // Nested function definitions are not allowed in Ralph.
        // If the input node is a function, return the node itself.
        Some(childNode.upcast(function))

      case ast: Ast.Positioned =>
        // For everything else, find the nearest function.
        ast
          .sourceIndex
          .flatMap {
            childNodeIndex =>
              childNode
                .walkParents
                .collectFirst {
                  case node @ Node(function: Ast.FuncDef[_], _) if function.sourceIndex.exists(_ contains childNodeIndex.index) =>
                    node.upcast(function)
                }
          }
    }

  /**
   * Navigate to local or built-in functions within the source code for the specified [[Ast.FuncId]].
   *
   * @param funcId     The [[Ast.FuncId]] of the function to locate.
   * @param sourceCode The source tree to search.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over all searched function definitions.
   */
  private def goToFunction(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.Positioned]] = {
    val functions =
      if (funcId.isBuiltIn)
        workspace.build.findDependency(DependencyID.BuiltIn) match {
          case Some(builtInWorkspace) =>
            WorkspaceSearcher.collectFunctions(builtInWorkspace.parsed)

          case None =>
            // there is no dependency on the built-in library with this workspace,
            // but maybe the workspace itself is a built-in library that has usages within itself.
            // TODO: Each workspace build should contain a DependencyID,
            //       so this collection of workspace functions runs only when required.
            WorkspaceSearcher
              .collectAllFunctions(workspace)
              .filter(_.ast.id.isBuiltIn)
        }
      else
        WorkspaceSearcher.collectFunctions(
          sourceCode = sourceCode,
          workspace = workspace
        )

    findFuncSignature(
      funcId = funcId,
      functions = functions
    )
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
                            Some(SourceLocation.Node(call, code)) // Matched! Both the `funcId` and `typeId` are a match.
                          else
                            None
                      }
                }
          }
      }
      .flatten

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
          SourceLocation.Node(exp, sourceCode)

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          SourceLocation.Node(funcCall, sourceCode)
      }

  /**
   * Navigate to all function implementations for the given function ID.
   *
   * @param functionId The function ID to search.
   * @param typeExpr   The expression containing the type information on which this function is invoked.
   * @param workspace  The workspace containing all source code.
   * @return An iterator containing all function implementations.
   */
  private def goToFunctionImplementation(
      functionId: Ast.FuncId,
      typeExpr: Ast.Expr[_],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Node[Ast.Positioned]] =
    typeExpr.getCachedType() match {
      case Some(types) =>
        val allFunctions =
          WorkspaceSearcher.collectFunctions(
            types = types,
            workspace = workspace
          )

        findFuncSignature(
          funcId = functionId,
          functions = allFunctions
        )

      case None =>
        logger.info(s"Go-to definition unsuccessful: Type inference unresolved for function '${functionId.name}'. Check for syntax or compilation errors.")
        Iterator.empty
    }

  /**
   * Finds the function signatures with the given id, transforming the function definitions to function signatures.
   *
   * @param funcId    The function id to search.
   * @param functions The functions to search within.
   * @return The matching function signatures.
   */
  private def findFuncSignature(
      funcId: Ast.FuncId,
      functions: Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]]): Iterator[SourceLocation.Node[Ast.Positioned]] =
    functions
      .filter(_.ast.id == funcId)
      .map {
        funcDef =>
          SourceLocation.Node(
            ast = AstExtra.funcSignature(funcDef.ast),
            source = funcDef.source
          )
      }

}
