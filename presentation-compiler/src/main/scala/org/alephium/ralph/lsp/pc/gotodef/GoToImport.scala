package org.alephium.ralph.lsp.pc.gotodef

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
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
        )

      case None =>
        ArraySeq.empty
    }

  def goTo(cursorIndex: Int,
           dependencyDir: Path,
           dependency: WorkspaceState.Compiled,
           importStatement: Tree.Import): ArraySeq[URI] =
    importStatement.path match {
      case Some(importPath) =>
        val goToSource =
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

        // return absolute paths for all files
        goToSource flatMap {
          sourceCode =>
            sourceCode.importIdentifier map {
              importIdentifier =>
                val importPath = importIdentifier.string.name.value
                val fullFilePath = dependencyDir resolve s"$importPath.${CompilerAccess.RALPH_FILE_EXTENSION}"
                fullFilePath.toUri
            }
        }

      case None =>
        ArraySeq.empty
    }
}
