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

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.{Type, Ast}
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState, SourceCodeSearcher}
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

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
    if (URIUtil.contains(workspace.build.contractURI, fileURI) && URIUtil.getFileExtension(fileURI) == CompilerAccess.RALPH_FILE_EXTENSION) {
      val parsedOrError =
        SourceCodeSearcher.findParsed(
          fileURI = fileURI,
          sourceCode = workspace.sourceCode
        )

      Some(parsedOrError)
    } else {
      None
    }

  /**
   * Collects all parent source implementations inherited by the given source tree.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectInheritedParents(
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Seq[SourceLocation.Code] = {
    val allInScopeCode =
      collectTrees(workspace = workspace, includeNonImportedCode = false)

    val inheritancesInScope =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = allInScopeCode
      )

    inheritancesInScope :+ sourceCode
  }

  /**
   * Collects all children implementing or extending the given
   * source tree and public contracts/structs.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectImplementingChildren(
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Seq[SourceLocation.Code] = {
    val allInScopeCode =
      collectTrees(workspace, includeNonImportedCode = false)

    val inheritancesInScope =
      SourceCodeSearcher.collectImplementingChildren(
        source = sourceCode,
        allSource = allInScopeCode
      )

    inheritancesInScope :+ sourceCode
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
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] = {
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
  def collectFunctions(workspace: WorkspaceState.Parsed): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] =
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
      sourceCode: SourceLocation.Code,
      workspace: WorkspaceState.IsSourceAware): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] =
    collectInheritedParents(sourceCode, workspace)
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
      includeNonImportedCode: Boolean): Iterator[SourceLocation.Node[Ast.TypeId]] = {
    val trees = collectTrees(workspace, includeNonImportedCode)
    SourceCodeSearcher.collectTypes(trees.iterator)
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
      includeNonImportedCode: Boolean): ArraySeq[SourceLocation.Code] = {
    // fetch the `std` dependency
    val stdSourceParsedCode =
      workspace
        .build
        .findDependency(DependencyID.Std)
        .to(ArraySeq)
        .flatMap(_.sourceCode.map(_.parsed))

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

    // The entire local local dev workspace source-code is available.
    val workspaceTrees =
      SourceCodeSearcher.collectSourceTrees(workspaceCode)

    workspaceTrees ++ allImportedCode
  }

}
