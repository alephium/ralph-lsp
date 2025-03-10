// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

import scala.collection.immutable.ArraySeq

/**
 * Result type for the function [[WorkspaceSearcher.collectInheritedParentsSoft]].
 *
 * @param parentTrees The resulting parent trees within the current workspace.
 * @param allTrees   All trees in scope within the current workspace.
 */
case class InheritedParentsResultSoft(
    parentTrees: ArraySeq[SourceLocation.CodeSoft],
    allTrees: ArraySeq[SourceLocation.CodeSoft])
