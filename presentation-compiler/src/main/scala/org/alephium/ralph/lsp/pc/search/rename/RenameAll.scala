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

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

import scala.collection.mutable.ListBuffer

/** Rename all references */
private object RenameAll extends StrictImplicitLogging {

  /**
   * Searches for related tokens that can be renamed for the token at the given cursor index.
   *
   * @param cursorIndex The index where this operation is performed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace state where the source-code is located.
   * @return Source locations of the tokens to be renamed.
   */
  def rename(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.Rename] = {
    val references =
      collectReferences(
        cursorIndex = cursorIndex,
        sourceCode = sourceCode,
        workspace = workspace
      )

    val (cannotRename, canRename) =
      references partition {
        ref =>
          // Changes must be within the developer's workspace. Cannot change dependencies.
          isRenamingDisallowed(
            ref = ref,
            build = workspace.build
          )
      }

    if (cannotRename.isEmpty) {
      canRename.iterator
    } else {
      // contains tokens that cannot be renamed
      val cannotRenameURIs       = cannotRename.map(_.parsed.fileURI)
      val cannotRenameURIStrings = cannotRenameURIs.mkString(", ")
      logger.info(s"Operation blocked: Renaming within files outside the active workspace is not allowed. Affected files: $cannotRenameURIStrings")
      Iterator.empty
    }
  }

  /**
   * Searches for related symbols that should be renamed following the renaming occurring
   * on the symbol which is at the given cursor index.
   *
   * @param cursorIndex The index where this operation is performed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   * @param workspace   The workspace state where the source-code is located.
   * @return Source locations of the tokens to be renamed.
   */
  private def collectReferences(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterable[SourceLocation.Rename] = {
    // collects all nodes that must be renamed
    val nodesToRename =
      ListBuffer.empty[(SourceLocation.Rename, SourceIndex)]

    // settings to run go-to-references on
    val searchSettings =
      GoToRefSetting(
        includeDeclaration = true,
        includeTemplateArgumentOverrides = true,
        includeEventFieldReferences = false // do not rename event field references
      )

    /** Start collect the nodes to rename */
    def runCollect(
        cursorIndex: Int,
        sourceCode: SourceCodeState.Parsed): Unit =
      CodeProvider
        .goToReferences
        .search(
          cursorIndex = cursorIndex,
          sourceCode = sourceCode,
          workspace = workspace,
          searchSettings = searchSettings
        )
        .foreach {
          case ref: SourceLocation.ImportName =>
            if (!nodesToRename.contains((ref, ref.name.index)))
              nodesToRename addOne (ref, ref.name.index)

          case ref @ SourceLocation.Node(ast, _) =>
            ast
              .sourceIndex // Nodes without SourceIndex cannot be renamed.
              .filter {
                sourceIndex =>
                  // ensure that nodes are only processed once
                  !nodesToRename.contains((ref, sourceIndex))
              }
              .foreach {
                sourceIndex =>
                  nodesToRename addOne (ref, sourceIndex)

                  runCollect(
                    cursorIndex = sourceIndex.from,
                    sourceCode = ref.source.parsed
                  )
              }
        }

    runCollect(
      cursorIndex = cursorIndex,
      sourceCode = sourceCode
    )

    nodesToRename.map(_._1)
  }

  /**
   * Checks if the given go-to reference cannot be renamed.
   *
   * @param ref   The reference to check for remaining restrictions.
   * @param build The current workspace build.
   * @return True if renaming is disallowed, false otherwise.
   */
  private def isRenamingDisallowed(
      ref: SourceLocation.Rename,
      build: BuildState.Compiled): Boolean = {
    val isOutsideWorkspace =
      !URIUtil.contains(
        parent = build.workspaceURI,
        child = ref.parsed.fileURI
      )

    def isInDependencyPath =
      URIUtil.contains(
        parent = build.dependencyPath,
        child = ref.parsed.fileURI
      )

    isOutsideWorkspace || isInDependencyPath
  }

}
