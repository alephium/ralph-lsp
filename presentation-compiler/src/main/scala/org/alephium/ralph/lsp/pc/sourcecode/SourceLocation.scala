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

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.ast.Tree

sealed trait SourceLocation

object SourceLocation {

  /**
   * Represents a single source tree ([[Tree.Source]]) within a source file ([[SourceCodeState.Parsed]]),
   * which can contain multiple source trees such as contracts, scripts etc.
   *
   * @param tree   The source tree within the parsed source file.
   * @param parsed The source file containing the source tree.
   */
  case class Code(
      tree: Tree.Source,
      parsed: SourceCodeState.Parsed)
    extends SourceLocation

}
