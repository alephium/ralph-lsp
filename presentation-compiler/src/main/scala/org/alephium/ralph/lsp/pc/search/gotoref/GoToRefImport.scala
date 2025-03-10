// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
