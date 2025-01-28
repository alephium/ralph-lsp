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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, WorkspaceSearcher}

import scala.collection.immutable.ArraySeq

private object GoToRefImport {

  def goTo(
      cursorIndex: Int,
      importStatement: Tree.Import,
      workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceLocation.ImportName] =
    importStatement.path match {
      case Some(importPath) =>
        if (importPath.folder.index contains cursorIndex) // check: is the cursor on a folder
          WorkspaceSearcher
            .collectAllParsed(workspace)
            .flatMap {
              parsed =>
                parsed.astStrict.statements.collect {
                  case Tree.Import(_, Some(thisPath), _) if importPath.folder.value == thisPath.folder.value =>
                    SourceLocation.ImportName(thisPath.folder, parsed)
                }
            }
        else if (importPath.file.index contains cursorIndex) // check: is the cursor for a file
          WorkspaceSearcher
            .collectAllParsed(workspace)
            .flatMap {
              parsed =>
                parsed.astStrict.statements.collect {
                  case Tree.Import(_, Some(thisPath), _) if importPath.folder.value == thisPath.folder.value && importPath.file.value == thisPath.file.value =>
                    SourceLocation.ImportName(thisPath.file, parsed)
                }
            }
        else
          ArraySeq.empty

      case None =>
        ArraySeq.empty
    }

}
