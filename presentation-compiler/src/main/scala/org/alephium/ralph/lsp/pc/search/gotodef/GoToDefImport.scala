// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
      importStatement: Tree.Import): ArraySeq[SourceLocation.GoToDefStrict] =
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
