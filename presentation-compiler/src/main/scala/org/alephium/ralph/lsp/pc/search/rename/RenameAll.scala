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

import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

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
    val (cannotRename, canRename) =
      CodeProvider
        .goToReferences
        .search(
          cursorIndex = cursorIndex,
          sourceCode = sourceCode,
          workspace = workspace,
          searchSettings = GoToRefSetting(
            includeDeclaration = true,
            includeTemplateArgumentOverrides = true
          )
        )
        .partition {
          ref =>
            // Changes must be within the developer's workspace. Cannot change dependencies.
            isRenamingDisallowed(
              ref = ref,
              build = workspace.build
            )
        }

    if (cannotRename.isEmpty) {
      canRename
    } else {
      // contains tokens that cannot be renamed
      val cannotRenameURIs       = cannotRename.map(_.parsed.fileURI)
      val cannotRenameURIStrings = cannotRenameURIs.mkString(", ")
      logger.info(s"Operation blocked: Renaming within files outside the active workspace is not allowed. Affected files: $cannotRenameURIStrings")
      Iterator.empty
    }
  }

  /**
   * Checks if the given go-to reference cannot be renamed.
   *
   * @param ref   The reference to check for remaining restrictions.
   * @param build The current workspace build.
   * @return True if renaming is disallowed, false otherwise.
   */
  private def isRenamingDisallowed(
      ref: SourceLocation.GoToRef,
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
