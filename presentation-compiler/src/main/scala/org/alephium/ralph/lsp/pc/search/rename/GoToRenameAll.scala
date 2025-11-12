// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.rename

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.URIUtil

import scala.collection.mutable.ListBuffer

/** Rename all references */
private object GoToRenameAll extends StrictImplicitLogging {

  /**
   * Searches for related symbols that can be renamed for the symbol at the given cursor index.
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
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToRenameSoft] = {
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
      // contains symbols that cannot be renamed
      val cannotRenameURIs       = cannotRename.map(_.parsed.fileURI)
      val cannotRenameURIStrings = cannotRenameURIs.mkString(", ")
      logger.info(s"Operation blocked: Renaming within files outside the active workspace is not allowed. Affected files: $cannotRenameURIStrings")
      Iterator.empty
    }
  }

  /**
   * Searches for related symbols that should be renamed following the renaming occurring
   * on the symbol located at the given cursor index.
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
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterable[SourceLocation.GoToRenameSoft] = {
    // collects all nodes that must be renamed
    val nodesToRename =
      ListBuffer.empty[(SourceLocation.GoToRenameSoft, SourceIndex)]

    // settings to run go-to-references on
    val searchSettings =
      GoToRefSetting(
        includeDeclaration = true,
        includeTemplateArgumentOverrides = true,
        includeEventFieldReferences = false, // do not rename event field references
        goToDefSetting = GoToDefSetting(
          includeAbstractFuncDef = true,
          includeInheritance = true
        )
      )

    // Start collect the nodes to rename
    def runCollect(
        cursorIndex: Int,
        sourceCode: SourceCodeState.IsParsedAndCompiled): Unit =
      CodeProvider
        .goToRef
        .searchLocal(
          cursorIndex = cursorIndex,
          sourceCode = sourceCode.toIsParsed,
          workspace = workspace,
          searchSettings = (SoftAST, searchSettings)
        )
        .foreach {
          case ref: SourceLocation.ImportName =>
            if (!nodesToRename.contains((ref, ref.name.index)))
              nodesToRename addOne (ref, ref.name.index)

          case ref @ SourceLocation.NodeSoft(ast, _) =>
            val key = (ref, ast.index)
            if (!nodesToRename.contains(key)) { // ensure that nodes are only processed once
              nodesToRename addOne key

              runCollect(
                cursorIndex = ast.index.from,
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
      ref: SourceLocation.GoToRenameSoft,
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
