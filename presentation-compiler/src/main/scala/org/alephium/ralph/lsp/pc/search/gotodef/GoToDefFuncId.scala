// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

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
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    funcIdNode.parent match { // take one step up to check the type of function call.
      case Some(parent) =>
        parent match {
          case Node(callExpr: Ast.CallExpr[_], _) if callExpr.id == funcIdNode.data =>
            // The user selected on a local function. Take 'em there!
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
            goToFunctionDefinitions(
              funcId = funcDef.id,
              sourceCode = sourceCode,
              workspace = workspace,
              settings = settings
            )

          case Node(ast, _) =>
            logger.error(s"GoTo not implemented for parent node '${ast.getClass.getSimpleName}' at source index '${ast.sourceIndex}'")
            Iterator.empty
        }

      case None =>
        logger.error(s"Parent node not found for AST '${funcIdNode.data.getClass.getSimpleName}' at source index '${funcIdNode.data.sourceIndex}'")
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
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] = {
    val functions =
      if (funcId.isBuiltIn)
        workspace.build.findDependency(DependencyID.BuiltIn) match {
          case Some(builtInWorkspace) =>
            WorkspaceSearcher
              .collectFunctions(builtInWorkspace.parsed)
              .filter(!_.parsed.isPrimitive(builtInWorkspace)) // Ignore functions primitive types

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
   * If [[GoToDefSetting.includeAbstractFuncDef]] is true,
   * this function behaves as if go-to-implementation was executed.
   *
   * For example, in the following case, executing go-to-definition on either of the
   * functions will result in both the abstract and the implementation being returned:
   * {{{
   *    Abstract Contract Parent() {
   *      fn >>function<<() -> ()
   *    }
   *
   *    Contract Child() extends Parent() {
   *      fn >>function()<< -> () { }
   *    }
   * }}}
   *
   * The final result will always include at least one [[SourceLocation.NodeStrict]] instance,
   * representing the input `funcId`.
   *
   * @param funcId     The [[Ast.FuncId]] of the function to locate.
   * @param sourceCode The source tree to search.
   * @param workspace  The workspace where this search was executed and where all the source trees exist.
   * @return An iterator over all searched function definitions.
   */
  private def goToFunctionDefinitions(
      funcId: Ast.FuncId,
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting): Iterator[SourceLocation.NodeStrict[Ast.FuncId]] = {
    // An iterator with only the input `FuncId` as the result
    def selfOnly() =
      Iterator(
        SourceLocation.NodeStrict(
          ast = funcId,
          source = sourceCode
        )
      )

    // Check: Should it include abstract functions definitions?
    if (settings.includeAbstractFuncDef) {
      val functions =
        WorkspaceSearcher
          .collectInheritanceHierarchy(
            sourceCode = sourceCode,
            workspace = workspace
          )
          .flatten()
          .flatMap(SourceCodeSearcher.collectFunctions)
          .filter {
            thisFuncDef =>
              // filter only the functions with the same name
              thisFuncDef.ast.id.name == funcId.name
          }

      // Check: Does an abstract function exist in the inheritance hierarchy?
      if (functions.exists(_.ast.bodyOpt.isEmpty))
        functions // Yes! Rename all functions.
          .iterator
          .map {
            funcDef =>
              SourceLocation.NodeStrict(
                ast = funcDef.ast.id,
                source = funcDef.source
              )
          }
      else
        selfOnly() // No! Rename only self.
    } else {
      selfOnly()
    }
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
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
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
      functions: Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]]): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    functions
      .filter(_.ast.id == funcId)
      .map {
        funcDef =>
          SourceLocation.NodeStrict(
            ast = funcDef.ast.id,
            source = funcDef.source
          )
      }

}
