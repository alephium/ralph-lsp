package org.alephium.ralph.lsp.pc.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI
import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object GoToImport {

  def goTo(cursorIndex: Int,
           dependencyDir: Path,
           dependency: Option[WorkspaceState.Compiled],
           importStatement: Tree.Import): ArraySeq[URI] =
    dependency match {
      case Some(dependency) =>
        goTo(
          cursorIndex = cursorIndex,
          dependencyDir = dependencyDir,
          dependency = dependency,
          importStatement = importStatement
        ).map(_.fileURI)

      case None =>
        ArraySeq.empty
    }

  private def goTo(cursorIndex: Int,
                   dependencyDir: Path,
                   dependency: WorkspaceState.Compiled,
                   importStatement: Tree.Import): ArraySeq[SourceCodeState.Compiled] =
    importStatement.path match {
      case Some(importPath) =>
        if (importPath.folder.index contains cursorIndex) // check: is the cursor on a folder
          dependency // return all files that are with the folder
            .sourceCode
            .filter(_.importIdentifier.exists(_.path.exists(_.folder.value == importPath.folder.value)))
        else if (importPath.file.index contains cursorIndex) // check: is the cursor for a file
          dependency // find the file
            .sourceCode
            .filter(_.importIdentifier.exists(_.string.name.value == importStatement.string.name.value))
        else
          ArraySeq.empty

      case None =>
        ArraySeq.empty
    }
}
