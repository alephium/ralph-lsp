// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.{Ast, Keyword}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefFuncId
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.Node

import scala.collection.immutable.ArraySeq

object FunctionBodyCompleter {

  /**
   * Provides all code completion suggestions within the scope of a function for the given index.
   *
   * @param cursorIndex     The index representing the cursor position where the request was submitted.
   * @param closestToCursor The node closest to the cursor index.
   * @param sourceCode      The source code where the completion is requested.
   * @param workspace       The workspace state containing the source code.
   * @return An iterator over code completion suggestions.
   */
  def suggest(
      cursorIndex: Int,
      closestToCursor: Node[Ast.Positioned, Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion] =
    GoToDefFuncId.goToNearestFuncDef(closestToCursor) match {
      case Some(functionNode) =>
        suggestInFunctionBody(
          cursorIndex = cursorIndex,
          functionNode = functionNode,
          sourceCode = sourceCode,
          workspace = workspace
        )

      case None =>
        // Functions are not mandatory for `TxScript`. When no actual function is found,
        // the request is assumed to have been executed within the `main` function of `TxScript`.
        // This is only an assumption because the main function does not contain a source index,
        // and therefore, the `cursorIndex` cannot be used to confirm this.
        SourceCodeSearcher
          .findTxScriptMainFunction(sourceCode)
          .map {
            mainFunction =>
              suggestInFunctionBody(
                cursorIndex = cursorIndex,
                functionNode = mainFunction,
                sourceCode = sourceCode,
                workspace = workspace
              )
          }
          .getOrElse(Iterator.empty)
    }

  /**
   * Provides suggestions available within the body of a function at the given position.
   *
   * FIXME (Performance): This function invokes function [[WorkspaceSearcher.collectTrees]]
   *                      multiple times over multiple functions which is inefficient.
   *
   * @param cursorIndex  The position where this search was executed.
   * @param functionNode The node representing the function where this search was executed.
   * @param sourceCode   The source code containing the given function.
   * @param workspace    The workspace containing the source code.
   * @return An iterator over suggestions available before the cursor position within the function.
   */
  private def suggestInFunctionBody(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion] = {
    // fetch suggestions local to this function
    val localFunctionSuggestions =
      suggestLocalFunctionVariables(
        cursorIndex = cursorIndex,
        functionNode = functionNode,
        sourceCode = sourceCode
      )

    // fetch suggestions available within this function due to inheritance.
    val inheritedSuggestions =
      suggestInheritedAPIs(
        sourceCode = sourceCode,
        workspace = workspace
      )

    val builtInFunctions =
      suggestBuiltinFunctions(workspace)

    val types =
      TypeCompleter.suggest(workspace)

    val globalConstants =
      suggestGlobalConstants(workspace)

    val keywords =
      suggestKeywords()

    localFunctionSuggestions ++
      inheritedSuggestions ++
      builtInFunctions ++
      types ++
      globalConstants ++
      keywords
  }

  /**
   * Suggests variables local to the given function.
   *
   * @param cursorIndex  The position where this request was executed.
   * @param functionNode The node representing the function where this request was executed.
   * @param sourceCode   The source code to which this function belongs.
   * @return An iterator of suggestions for function arguments and local variables
   *         within the function.
   */
  private def suggestLocalFunctionVariables(
      cursorIndex: Int,
      functionNode: Node[Ast.FuncDef[_], Ast.Positioned],
      sourceCode: SourceLocation.CodeStrict): Iterator[Suggestion.NodeAPI] =
    functionNode
      .walkDown
      .filter(_.data.sourceIndex.exists(_.from <= cursorIndex))
      .collect {
        case Node(argument: Ast.Argument, _) =>
          Suggestion.Argument(
            node = SourceLocation.NodeStrict(ast = argument, source = sourceCode),
            isTemplateArgument = false
          )

        case Node(data: Ast.VarDef[_], _) =>
          Suggestion.VarDef(SourceLocation.NodeStrict(data, sourceCode))
      }

  /**
   * Suggests public APIs available from inherited code.
   *
   * @param sourceCode The code where this function exists.
   * @param workspace  The workspace state where the source code is located.
   * @return An iterator over suggestions from inherited APIs.
   */
  private def suggestInheritedAPIs(
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.InheritedAPI] =
    WorkspaceSearcher
      .collectInheritedParents(
        sourceCode = sourceCode,
        workspace = workspace
      )
      .parentTrees
      .iterator
      .flatMap(suggestInheritedAPIs)

  /**
   * Suggests all public APIs from this code available to implementing contracts.
   *
   * @param sourceCode The source code that can be inherited.
   * @return An iterator over public APIs available to inherited code.
   */
  private def suggestInheritedAPIs(sourceCode: SourceLocation.CodeStrict): Iterator[Suggestion.InheritedAPI] =
    sourceCode
      .tree
      .rootNode
      .walkDown
      .collect {
        case node @ Node(argument: Ast.Argument, _) if node.parent.exists(_.data.isInstanceOf[Ast.ContractWithState]) =>
          // suggest template level arguments
          Suggestion.Argument(
            node = SourceLocation.NodeStrict(argument, sourceCode),
            isTemplateArgument = true
          )

        case Node(function: Ast.FuncDef[_], _) =>
          // suggest function names
          Suggestion.FuncDef(
            node = SourceLocation.NodeStrict(function, sourceCode),
            isBuiltIn = false
          )

        case Node(eventDef: Ast.EventDef, _) =>
          // suggest events
          Suggestion.EventDef(SourceLocation.NodeStrict(eventDef, sourceCode))

        case Node(enumDef: Ast.EnumDef[_], _) =>
          // suggest enums
          Suggestion.EnumDef(SourceLocation.NodeStrict(enumDef, sourceCode))

        case Node(constantVarDef: Ast.ConstantVarDef[_], _) =>
          // suggest constants
          Suggestion.ConstantVarDef(SourceLocation.NodeStrict(constantVarDef, sourceCode))

        case Node(mapDef: Ast.MapDef, _) =>
          // suggest maps
          Suggestion.MapDef(SourceLocation.NodeStrict(mapDef, sourceCode))
      }

  /**
   * Suggests built-in functions available to the workspace as a dependency.
   *
   * @param workspace The workspace that contains the built-in dependency.
   * @return Iterator over built-in functions.
   */
  private def suggestBuiltinFunctions(workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.FuncDef] =
    workspace.build.findDependency(DependencyID.BuiltIn) match {
      case Some(builtIn) =>
        WorkspaceSearcher
          .collectFunctions(builtIn.parsed)
          .map {
            node =>
              Suggestion.FuncDef(
                node = node,
                isBuiltIn = true
              )
          }

      case None =>
        Iterator.empty
    }

  /**
   * Suggests global constants within the given workspace.
   *
   * @param workspace The workspace to search within.
   * @return An iterator over suggestions for global constants.
   */
  private def suggestGlobalConstants(workspace: WorkspaceState.IsSourceAware): Iterator[Suggestion.ConstantVarDef] =
    WorkspaceSearcher
      .collectGlobalConstants(workspace)
      .map(Suggestion.ConstantVarDef)

  /**
   * Suggests keywords relevant to a function's body.
   */
  private def suggestKeywords(): ArraySeq[Suggestion.Keyword] =
    ArraySeq(
      Suggestion.Keyword.expression(Keyword.let),
      suggestLetMut(),
      Suggestion.Keyword.expression(Keyword.emit),
      Suggestion.Keyword.expression(Keyword.`return`),
      Suggestion.Keyword.expression(Keyword.`if`),
      Suggestion.Keyword.expression(Keyword.`while`),
      Suggestion.Keyword.value(Keyword.`true`),
      Suggestion.Keyword.value(Keyword.`false`),
      Suggestion.Keyword.value(Keyword.alph),
      Suggestion.Keyword.value(Keyword.ALPH_CAPS)
    )

  private def suggestLetMut(): Suggestion.Keyword = {
    val label = s"${Keyword.let.name} ${Keyword.mut.name}"
    Suggestion.Keyword(
      label = label,
      insert = s"$label ",
      detail = ""
    )
  }

}
