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

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import scala.collection.immutable.ArraySeq

private object GoToDefImport {

  def goTo(
      cursorIndex: Int,
      dependency: Option[WorkspaceState.Compiled],
      importStatement: Tree.Import): ArraySeq[SourceLocation.GoToDef] =
    dependency match {
      case Some(dependency) =>
        goTo(
          cursorIndex = cursorIndex,
          dependency = dependency,
          importStatement = importStatement
        ) map {
          code =>
            SourceLocation.File(code.parsed)
        }

      case None =>
        ArraySeq.empty
    }

  private def goTo(
      cursorIndex: Int,
      dependency: WorkspaceState.Compiled,
      importStatement: Tree.Import): ArraySeq[SourceCodeState.Compiled] =
    importStatement.path match {
      case Some(importPath) =>
        if (importPath.folder.index contains cursorIndex) // check: is the cursor on a folder
          dependency                                      // return all files that are within the folder
            .sourceCode
            .filter(_.importIdentifier.exists(_.path.exists(_.folder.value == importPath.folder.value)))
        else if (importPath.file.index contains cursorIndex) // check: is the cursor for a file
          dependency                                         // find the file
            .sourceCode
            .filter(_.importIdentifier.exists(_.string.name.value == importStatement.string.name.value))
        else
          ArraySeq.empty

      case None =>
        ArraySeq.empty
    }

}
