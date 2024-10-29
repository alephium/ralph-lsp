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

import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

import scala.collection.immutable.ArraySeq

/** All inheritance data for the [[self]] */
case class InheritanceHierarchyResult(
    parentTrees: ArraySeq[SourceLocation.Code],
    childTrees: ArraySeq[SourceLocation.Code],
    allTrees: ArraySeq[SourceLocation.Code],
    self: SourceLocation.Code) {

  def flatten(): ArraySeq[SourceLocation.Code] =
    ((parentTrees :+ self) ++ childTrees).distinct

}
