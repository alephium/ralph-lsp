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

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.{SourceTreeInScope, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

private object GoToFuncId {

  /**
   * Navigate to the definition of a function for the given [[Ast.FuncId]].
   *
   * @param funcIdNode The node representing the [[Ast.FuncId]] in the AST.
   * @param funcId     The [[Ast.FuncId]] of the function to find the definition for.
   * @param sourceCode The source-tree and its parsed source-code state, where this search was executed.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An array sequence containing the positioned ASTs of the searched function.
   */
  def goTo(
      funcIdNode: Node[Positioned],
      funcId: Ast.FuncId,
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware): Iterator[GoToLocation] =
    funcIdNode.parent match { // take one step up to check the type of function call.
      case Some(parent) =>
        parent match {
          case Node(callExpr: Ast.CallExpr[_], _) if callExpr.id == funcId =>
            // The user clicked on a local function. Take 'em there!
            goToFunction(
              funcId = callExpr.id,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
            goToFunction(
              funcId = funcCall.id,
              sourceCode = sourceCode,
              workspace = workspace
            )

          case Node(funcDef: Ast.FuncDef[_], _) if funcDef.id == funcId =>
            GoTo.implementingChildren(
              sourceCode = sourceCode,
              workspace = workspace,
              searcher = // search for function usages
                goToFunctionUsage(
                  funcId = funcDef.id,
                  _
                )
            )

          case Node(callExpr: Ast.ContractCallExpr, _) if callExpr.callId == funcId =>
            // TODO: The user clicked on a external function call. Take 'em there!
            Iterator.empty

          case _ =>
            Iterator.empty
        }

      case None =>
        Iterator.empty
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
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware): Iterator[GoToLocation] =
    if (funcId.isBuiltIn)
      goToBuiltInFunction(
        funcId = funcId,
        dependencyBuiltIn = workspace.build.findDependency(DependencyID.BuiltIn)
      )
    else
      GoTo.inheritedParents(
        sourceCode = sourceCode,
        workspace = workspace,
        searcher = goToLocalFunction(funcId, _)
      )

  /**
   * Navigate to the local function within the source code for the given [[Ast.FuncId]].
   *
   * @param funcId The [[Ast.FuncId]] of the local function to locate.
   * @param source The source tree to search within.
   * @return An array sequence containing all the local function definitions.
   */
  private def goToLocalFunction(
      funcId: Ast.FuncId,
      source: Tree.Source): Iterator[Ast.Positioned] =
    // TODO: Improve selection by checking function argument count and types.
    source.ast match {
      case Left(ast) =>
        ast
          .funcs
          .iterator
          .filter(_.id == funcId)
          .map {
            funcDef =>
              if (funcDef.bodyOpt.isEmpty)
                funcDef // we show the entire function definition to display the function signature.
              else
                // The function contains a body so return just the function id.
                // FIXME: There is still a need to display just the function signature.
                //        At the moment there is no AST type that provides just the function signature.
                funcDef.id
          }

      case Right(_) =>
        Iterator.empty
    }

  /**
   * Navigate to built-in functions identified by the given [[Ast.FuncId]] within a the dependant workspace.
   *
   * @param funcId            The build-in function id to search.
   * @param dependencyBuiltIn Dependant workspace containing built-in function source files.
   * @return A iterator over locations of the built-in functions within the compiled workspace.
   */
  private def goToBuiltInFunction(
      funcId: Ast.FuncId,
      dependencyBuiltIn: Option[WorkspaceState.Compiled]): Iterator[GoToLocation] =
    dependencyBuiltIn match {
      case Some(buildInWorkspace) =>
        buildInWorkspace
          .sourceCode
          .iterator // iterator over dependant source-code files
          .flatMap {
            compiled =>
              goToBuiltInFunction(
                funcId = funcId,
                builtInFunctions = compiled
              )
          }

      case None =>
        Iterator.empty
    }

  /**
   * Navigate to built-in functions identified by the given [[Ast.FuncId]] within a source file.
   *
   * @param funcId           The build-in function id to search.
   * @param builtInFunctions Compiled source file containing built-in functions.
   * @return A iterator over locations of the built-in functions within the compiled source file.
   */
  private def goToBuiltInFunction(
      funcId: Ast.FuncId,
      builtInFunctions: SourceCodeState.Compiled): Iterator[GoToLocation] =
    builtInFunctions
      .parsed
      .ast
      .statements
      .iterator
      .collect {
        case source: Tree.Source =>
          // search for the matching functionIds within the built-in source file.
          val builtInFunctionIDs =
            goToLocalFunction(
              funcId = funcId,
              source = source
            )

          GoToLocation(
            sourceCode = builtInFunctions.parsed,
            asts = builtInFunctionIDs
          )
      }
      .flatten

  /**
   * Navigate to all local function usage where the given function definition [[Ast.FuncDef]]
   * is invoked.
   *
   * @param funcId The [[Ast.FuncId]] of the [[Ast.FuncDef]] to locate calls for.
   * @param source The source tree to search within.
   * @return An iterator containing all the local function calls.
   */
  private def goToFunctionUsage(
      funcId: Ast.FuncId,
      source: Tree.Source): Iterator[Ast.Positioned] =
    source
      .rootNode
      .walkDown
      .collect {
        case Node(exp: Ast.CallExpr[_], _) if exp.id == funcId =>
          exp

        case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
          funcCall
      }

}
