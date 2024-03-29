package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

private object GoToFuncId {

  /**
   * Navigate to the definition of a function for the given [[Ast.FuncId]].
   *
   * @param funcIdNode The node representing the [[Ast.FuncId]] in the AST.
   * @param funcId     The [[Ast.FuncId]] of the function to find the definition for.
   * @param source     The source tree to search within.
   * @return An array sequence containing the positioned ASTs of the searched function.
   * */
  def goTo(funcIdNode: Node[Positioned],
           funcId: Ast.FuncId,
           source: Tree.Source,
           sourceCode: SourceCodeState.Parsed,
           dependencyBuiltIn: Option[WorkspaceState.Compiled]): Iterator[GoToLocation] =
    funcIdNode.parent match { // take one step up to check the type of function call.
      case Some(parent) =>
        parent match {
          case Node(callExpr: Ast.CallExpr[_], _) if callExpr.id == funcId =>
            // The user clicked on a local function. Take 'em there!
            goToFunction(
              funcId = callExpr.id,
              source = source,
              sourceCode = sourceCode,
              dependencyBuiltIn = dependencyBuiltIn
            )

          case Node(funcCall: Ast.FuncCall[_], _) if funcCall.id == funcId =>
            goToFunction(
              funcId = funcCall.id,
              source = source,
              sourceCode = sourceCode,
              dependencyBuiltIn = dependencyBuiltIn
            )

          case Node(funcDef: Ast.FuncDef[_], _) if funcDef.id == funcId =>
            goToFunctionUsage(
              funcId = funcDef.id,
              source = source
            ).flatMap(GoToLocation(_, sourceCode))

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
   * @param funcId            The [[Ast.FuncId]] of the function to locate.
   * @param source            The source tree to search within.
   * @param sourceCode        The source code corresponding to the source tree.
   * @param dependencyBuiltIn The dependency workspace containing built-in functions.
   * @return An iterator over all searched function definitions.
   */
  private def goToFunction(funcId: Ast.FuncId,
                           source: Tree.Source,
                           sourceCode: SourceCodeState.Parsed,
                           dependencyBuiltIn: Option[WorkspaceState.Compiled]): Iterator[GoToLocation] =
    if (funcId.isBuiltIn)
      goToBuiltInFunction(
        funcId = funcId,
        dependencyBuiltIn = dependencyBuiltIn
      )
    else
      goToLocalFunction(
        funcId = funcId,
        source = source
      ).flatMap(GoToLocation(_, sourceCode))

  /**
   * Navigate to the local function within the source code for the given [[Ast.FuncId]].
   *
   * @param funcId The [[Ast.FuncId]] of the local function to locate.
   * @param source The source tree to search within.
   * @return An array sequence containing all the local function definitions.
   * */
  private def goToLocalFunction(funcId: Ast.FuncId,
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
  private def goToBuiltInFunction(funcId: Ast.FuncId,
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
  private def goToBuiltInFunction(funcId: Ast.FuncId,
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
   * */
  private def goToFunctionUsage(funcId: Ast.FuncId,
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
