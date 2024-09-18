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
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

private[search] object GoToDefFuncId extends StrictImplicitLogging {

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

          case Node(call: Ast.ContractCallBase, _) if call.callId == funcIdNode.data =>
            goToFunctionImplementation(
              functionId = funcIdNode.data,
              typeExpr = call.obj,
              workspace = workspace
            )

          case Node(funcDef: Ast.FuncDef[_], _) if funcDef.id == funcIdNode.data =>
            Iterator(
              SourceLocation.Node(
                ast = funcDef,
                source = sourceCode
              )
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
