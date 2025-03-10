// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

import scala.collection.immutable.ArraySeq

/**
 * Result type for the function [[WorkspaceSearcher.collectImplementingChildren]].
 *
 * @param childTrees The resulting child trees within the current workspace.
 * @param allTrees   All trees in scope within the current workspace.
 */
case class ImplementingChildrenResult(
    childTrees: ArraySeq[SourceLocation.CodeStrict],
    allTrees: ArraySeq[SourceLocation.CodeStrict])
