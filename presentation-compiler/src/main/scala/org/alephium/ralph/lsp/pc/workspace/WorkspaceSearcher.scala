// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.utils.URIUtil
import org.alephium.ralph.lsp.utils.log.ClientLogger

import java.net.URI
import scala.collection.immutable.ArraySeq

/** Implements search functions that run on [[WorkspaceState.IsSourceAware]] */
object WorkspaceSearcher {

  /**
   * Find a parsed state [[SourceCodeState.Parsed]] for the given file URI.
   *
   * @param fileURI   The file URI of the parsed source-code.
   * @param workspace Current workspace.
   * @return - None: If this file does not support completion.
   *         - Right: If a parsed state was found.
   *         - Left: If the source-code is in one of the non-parsed states.
   */
  def findParsed(
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware): Option[Either[CompilerMessage.Error, SourceCodeState.Parsed]] =
    // file must belong to the workspace contractURI and must be a ralph source file
    if (isWorkspaceSourceFile(fileURI, workspace)) {
      val parsedOrError =
        SourceCodeSearcher.findParsed(
          fileURI = fileURI,
          sourceCode = workspace.sourceCode
        )

      Some(parsedOrError)
    } else {
      None
    }

  def findIsParsed(
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware): Option[Either[CompilerMessage.Error, SourceCodeState.IsParsed]] =
    // file must belong to the workspace contractURI and must be a ralph source file
    if (isWorkspaceSourceFile(fileURI, workspace)) {
      val parsedOrError =
        SourceCodeSearcher.findIsParsed(
          fileURI = fileURI,
          sourceCode = workspace.sourceCode
        )

      Some(parsedOrError)
    } else {
      None
    }

  /**
   * Collects all parent and child source implementations inherited and implemented by the given source tree.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectInheritanceHierarchy(
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): InheritanceHierarchyResult = {
    val allInScopeCode =
      collectTrees(workspace = workspace, includeNonImportedCode = false)

    val parents =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = allInScopeCode
      )

    val children =
      SourceCodeSearcher.collectImplementingChildren(
        source = sourceCode,
        allSource = allInScopeCode
      )

    InheritanceHierarchyResult(
      parentTrees = parents,
      childTrees = children,
      allTrees = allInScopeCode,
      self = sourceCode
    )
  }

  /**
   * Collects all parent source implementations inherited by the given source tree.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectInheritedParents(
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): InheritedParentsResult = {
    val allInScopeCode =
      collectTrees(workspace = workspace, includeNonImportedCode = false)

    val inheritancesInScope =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = allInScopeCode
      )

    val parents =
      inheritancesInScope :+ sourceCode

    InheritedParentsResult(
      parentTrees = parents,
      allTrees = allInScopeCode
    )
  }

  /**
   * Collects all parent source implementations inherited by the given source tree, excluding itself.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectInheritedParentsSoft(
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): InheritedParentsResultSoft = {
    val allInScopeCode =
      collectTreesSoft(
        workspace = workspace,
        includeNonImportedCode = false
      )

    val parents =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = allInScopeCode
      )

    InheritedParentsResultSoft(
      parentTrees = parents,
      allTrees = allInScopeCode
    )
  }

  /**
   * Collects all children implementing or extending the given
   * source tree and public contracts/structs.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return An [[ImplementingChildrenResult]] instance that stores the resulting
   *         child source trees and all trees in scope of the current workspace.
   */
  def collectImplementingChildren(
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): ImplementingChildrenResult = {
    val allInScopeCode =
      collectTrees(workspace, includeNonImportedCode = false)

    val inheritancesInScope =
      SourceCodeSearcher.collectImplementingChildren(
        source = sourceCode,
        allSource = allInScopeCode
      )

    val children = inheritancesInScope :+ sourceCode

    ImplementingChildrenResult(
      childTrees = children,
      allTrees = allInScopeCode
    )
  }

  /**
   * Collects all functions from trees with the given types.
   *
   * @param types     The types of trees from which to collect functions.
   * @param workspace The workspace state containing all source code to search from.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(
      types: Seq[Type],
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] = {
    val workspaceTrees =
      collectTrees(workspace, includeNonImportedCode = false)

    SourceCodeSearcher.collectFunctions(
      types = types,
      workspaceSource = workspaceTrees
    )
  }

  /**
   * Collects ALL function definitions within the provided parsed workspace state.
   *
   * @param workspace The parsed workspace state from which to collect function definitions.
   * @return An iterator containing all function implementations.
   * @note This function is mainly useful for the built-in library
   *       because all built-in functions are available throughout the workspace.
   *       Consider using other [[collectFunctions]] functions for more targeted collections.
   */
  def collectFunctions(workspace: WorkspaceState.Parsed): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
    collectAllFunctions(workspace)

  /**
   * Collects ALL function definitions within the provided parsed workspace state.
   *
   * Note: Collecting all functions within a large workspace can be expensive.
   *
   * @param workspace The parsed workspace state from which to collect function definitions.
   * @return An iterator containing all function implementations.
   */
  def collectAllFunctions(workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
    collectTrees(workspace, includeNonImportedCode = false)
      .iterator
      .flatMap(SourceCodeSearcher.collectFunctions)

  /**
   * Collects all function definitions in scope for the provided source code.
   *
   * @param sourceCode The source code from which to collect function definitions.
   * @param workspace  The workspace state that is source aware.
   * @return An iterator containing all function implementations, including inherited ones.
   */
  def collectFunctions(
      sourceCode: SourceLocation.CodeStrict,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
    collectInheritedParents(sourceCode, workspace)
      .parentTrees
      .iterator
      .flatMap(SourceCodeSearcher.collectFunctions)

  /**
   * Collects all types available in the provided workspace.
   *
   * @param workspace The workspace to search for types.
   * @return An iterator containing type identifiers.
   */
  def collectTypes(
      workspace: WorkspaceState.IsSourceAware,
      includeNonImportedCode: Boolean): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] = {
    val trees = collectTrees(workspace, includeNonImportedCode)
    SourceCodeSearcher.collectTypes(trees.iterator)
  }

  /**
   * Collects source tree locations for the given types.
   *
   * @param types     The types to search.
   * @param workspace The workspace to search for types.
   * @return Type identifiers matching the searched types and their respective source trees.
   */
  def collectTypes(
      types: Seq[Type],
      workspace: WorkspaceState.IsSourceAware): ArraySeq[(Ast.TypeId, SourceLocation.CodeStrict)] = {
    val workspaceTrees = collectAllTrees(workspace)
    SourceCodeSearcher.collectTypes(
      types = types,
      workspaceSource = workspaceTrees
    )
  }

  /**
   * Collects all global constants within the provided parsed workspace state.
   *
   * @param workspace The parsed workspace state from which to collect global constants.
   * @return An iterator containing all global constants.
   */
  def collectGlobalConstants(workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.NodeStrict[Ast.ConstantVarDef[_]]] = {
    val trees = collectTrees(workspace, includeNonImportedCode = false)
    SourceCodeSearcher.collectGlobalConstants(trees.iterator)
  }

  /**
   * Collects ALL [[org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST]] sources from dependency with the given [[DependencyID]].
   *
   * @param build The build containing the dependency.
   * @return All Parsed source files in all workspaces.
   */
  def collectAllDependencyTreesSoft(
      dependencyID: DependencyID,
      build: BuildState.Compiled
    )(implicit logger: ClientLogger): Option[(WorkspaceState.Compiled, ArraySeq[SourceLocation.CodeSoft])] =
    build.findDependency(dependencyID) map {
      dependency =>
        (dependency, collectAllTreesSoft(dependency))
    }

  /**
   * Collects all in-scope workspace source trees.
   *
   * @param workspace The workspace to collect source trees for.
   * @return Parsed source files in scope.
   */
  def collectAllTrees(workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceLocation.CodeStrict] =
    collectTrees(
      workspace = workspace,
      includeNonImportedCode = false
    )

  def collectAllTreesSoft(workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] =
    collectTreesSoft(
      workspace = workspace,
      includeNonImportedCode = false
    )

  /**
   * Collects ALL parsed source code from the workspace and dependencies.
   *
   * @param workspace The workspace to collect parsed code for.
   * @return All Parsed source files in all workspaces.
   */
  def collectAllParsed(workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceCodeState.Parsed] = {
    val dependencySources = workspace.build.dependencies.flatMap(_.sourceCode)
    val workspaceSources  = workspace.sourceCode
    val allSources        = workspaceSources ++ dependencySources
    SourceCodeSearcher.collectParsed(allSources)
  }

  /**
   * Collects all parsed source files, excluding `std` dependency source files
   * that are not imported.
   *
   * @param workspace              The workspace with dependencies.
   * @param includeNonImportedCode If true, includes dependency code that is not imported,
   *                               otherwise, excludes non-imported dependency code.
   * @return Parsed source files in scope.
   */
  private def collectTrees(
      workspace: WorkspaceState.IsSourceAware,
      includeNonImportedCode: Boolean): ArraySeq[SourceLocation.CodeStrict] = {
    val (stdSourceParsedCode, primitives) =
      collectDependencySourceCode(workspace.build)

    // collect all parsed source-files
    val workspaceCode =
      SourceCodeSearcher.collectParsed(workspace.sourceCode)

    val importedCode =
      if (includeNonImportedCode) {
        stdSourceParsedCode
      } else {
        // collect all import statements
        val importStatements =
          SourceCodeSearcher
            .collectImportStatements(workspaceCode)
            .map(_.string.value)

        // filter out std files that are not imported
        stdSourceParsedCode filter {
          stdCode =>
            stdCode
              .importIdentifier
              .exists {
                stdImportIdentifier =>
                  importStatements contains stdImportIdentifier.string.value
              }
        }
      }

    // Pull in all inherited source-files for the used import statements.
    val importedInheritedParentTrees =
      SourceCodeSearcher.collectInheritedParentsForAll(
        sourceCode = importedCode,
        workspace = stdSourceParsedCode
      )

    // collect all imported code including the inherited code.
    val allImportedCode =
      (SourceCodeSearcher.collectSourceTrees(importedCode) ++ importedInheritedParentTrees).distinct

    // The entire local dev workspace source-code is available.
    val workspaceTrees =
      SourceCodeSearcher.collectSourceTrees(workspaceCode)

    val primitiveTrees =
      SourceCodeSearcher.collectSourceTrees(primitives)

    workspaceTrees ++ allImportedCode ++ primitiveTrees
  }

  /**
   * Collects all parsed source files, excluding `std` dependency source files
   * that are not imported.
   *
   * @param workspace              The workspace with dependencies.
   * @param includeNonImportedCode If true, includes dependency code that is not imported,
   *                               otherwise, excludes non-imported dependency code.
   * @return Parsed source files in scope.
   */
  private def collectTreesSoft(
      workspace: WorkspaceState.IsSourceAware,
      includeNonImportedCode: Boolean
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] = {
    val (stdSourceParsedCode, primitives) =
      collectDependencySourceCode(workspace.build)

    // collect all parsed source-files
    val workspaceCode =
      SourceCodeSearcher.collectIsParsed(workspace.sourceCode)

    val importedCode =
      if (includeNonImportedCode) {
        stdSourceParsedCode
      } else {
        // collect all import statements
        val importStatements =
          SourceCodeSearcher
            .collectImportStatementsSoft(workspaceCode)
            .flatMap(_.string)
            .map(_.toCode())

        // filter out std files that are not imported
        stdSourceParsedCode filter {
          stdCode =>
            stdCode
              .importIdentifier
              .exists {
                stdImportIdentifier =>
                  importStatements contains stdImportIdentifier.string.value
              }
        }
      }

    // Pull in all inherited source-files for the used import statements.
    val importedInheritedParentTrees =
      SourceCodeSearcher.collectInheritedParentsForAllSoft(
        sourceCode = importedCode,
        workspace = stdSourceParsedCode
      )

    // collect all imported code including the inherited code.
    val allImportedCode =
      (SourceCodeSearcher.collectSourceTreesSoft(importedCode) ++ importedInheritedParentTrees).distinct

    // The entire local dev workspace source-code is available.
    val workspaceTrees =
      SourceCodeSearcher.collectSourceTreesSoft(workspaceCode)

    val primitiveTrees =
      SourceCodeSearcher.collectSourceTreesSoft(primitives)

    workspaceTrees ++ allImportedCode ++ primitiveTrees
  }

  /**
   * Collects all parsed and searchable source files from the dependency workspace.
   *
   * @param build The build containing the dependencies.
   * @return A tuple:
   *          - All parsed source files from the standard `std` library.
   *          - The parsed primitive source file contained in the `builtin` dependency.
   */
  private def collectDependencySourceCode(build: BuildState.Compiled): (ArraySeq[SourceCodeState.Parsed], ArraySeq[SourceCodeState.Parsed]) = {
    // fetch the `std` dependency
    val stdSourceParsedCode =
      build
        .findDependency(DependencyID.Std)
        .to(ArraySeq)
        .flatMap(_.sourceCode.map(_.parsed))

    // fetch the `primitives.ral` file
    val primitives =
      build
        .findDependency(DependencyID.BuiltIn)
        .to(ArraySeq)
        .flatMap {
          builtIn =>
            builtIn
              .sourceCode
              .map(_.parsed)
              .filter(_.isPrimitive(builtIn))
        }

    (stdSourceParsedCode, primitives)
  }

  /**
   * Checks whether the given file URI belongs to the given workspace and is a Ralph source file.
   *
   * @param fileURI   The URI of the file to check.
   * @param workspace The workspace to verify.
   * @return `true` if the file is a Ralph source file and belongs to the workspace, else `false`.
   */
  private def isWorkspaceSourceFile(
      fileURI: URI,
      workspace: WorkspaceState.IsSourceAware): Boolean =
    URIUtil.contains(workspace.build.contractURI, fileURI) &&
      CompilerAccess.isRalphFileExtension(fileURI)

}
