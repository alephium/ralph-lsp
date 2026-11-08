// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.cache

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{Dependency, DependencyID}
import org.alephium.ralph.lsp.utils.URIUtil
import org.alephium.ralph.lsp.utils.log.ClientLogger

import scala.collection.immutable.ArraySeq

object WorkspaceSearchCache {

  def apply(workspaceState: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): WorkspaceSearchCache =
    new WorkspaceSearchCache(workspaceState)

}

/**
 * Caches data created and reused when searching local, inherited and global scopes.
 *
 * A single search may access these scopes multiple times.
 *
 * @param workspace The workspace being search.
 */
final class WorkspaceSearchCache(val workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger) {

  /**
   * All workspace source trees in-scope.
   */
  lazy val trees: ArraySeq[SourceLocation.CodeSoft] =
    WorkspaceSearcher.collectAllTreesSoft(workspace)

  /**
   * All built-in trees excluding the primitives.
   */
  lazy val builtInTrees: ArraySeq[SourceLocation.CodeSoft] =
    WorkspaceSearcher.collectAllDependencyTreesSoft(
      dependencyID = DependencyID.BuiltIn,
      build = workspace.build
    ) match {
      case Some((builtIn, builtInTrees)) =>
        // Primitives should not be included within inheritance search.
        builtInTrees.filter(
          tree => !tree.parsed.isPrimitive(builtIn) && URIUtil.getFileName(tree.parsed.fileURI) != Dependency.ANNOTATION_FILE_NAME
        )

      case None =>
        ArraySeq.empty
    }

}
