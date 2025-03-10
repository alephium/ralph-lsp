// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

import scala.collection.immutable.ArraySeq

/** All inheritance data for the [[self]] */
case class InheritanceHierarchyResult(
    parentTrees: ArraySeq[SourceLocation.CodeStrict],
    childTrees: ArraySeq[SourceLocation.CodeStrict],
    allTrees: ArraySeq[SourceLocation.CodeStrict],
    self: SourceLocation.CodeStrict) {

  def flatten(): ArraySeq[SourceLocation.CodeStrict] =
    ((parentTrees :+ self) ++ childTrees).distinct

}
