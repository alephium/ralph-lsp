package org.alephium.ralph.lsp.pc.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.StrictImplicitLogging
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}

import java.net.URI
import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object GoToDefinition extends StrictImplicitLogging {

  /**
   * Provides go-to definition for the cursor position within the current workspace state.
   *
   * @param line          Line position in a document (zero-based).
   * @param character     Character offset on a line in a document (zero-based).
   * @param fileURI       The text document's uri.
   * @param dependencyDir Directory where the dependency code exists.
   * @param workspace     Current workspace state.
   * @return
   */
  def goTo(line: Int,
           character: Int,
           fileURI: URI,
           dependencyDir: Path,
           workspace: WorkspaceState.IsSourceAware): Option[Either[CompilerMessage.Error, ArraySeq[URI]]] =
    Workspace.findParsed(
      fileURI = fileURI,
      workspace = workspace
    ) map {
      result =>
        result map {
          parsed =>
            goTo(
              line = line,
              character = character,
              dependencyDir = dependencyDir,
              workspace = workspace,
              sourceCode = parsed
            )
        }
    }

  private def goTo(line: Int,
                   character: Int,
                   dependencyDir: Path,
                   workspace: WorkspaceState.IsSourceAware,
                   sourceCode: SourceCodeState.Parsed): ArraySeq[URI] = {
    // fetch the requested index from line number and character number.
    val cursorIndex =
      StringUtil.computeIndex(
        lines = sourceCode.codeLines,
        line = line,
        character = character
      )

    // find the statement where this cursorIndex sits.
    sourceCode.ast.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import statement completion
            GoToImport.goTo(
              cursorIndex = cursorIndex,
              dependencyDir = dependencyDir,
              workspace.build.dependency,
              importStatement = importStatement
            )

          case _: Tree.Source =>
            ArraySeq.empty // TODO: Provide source level completion.
        }

      case None =>
        // TODO: Provide top level completion.
        ArraySeq.empty
    }
  }
}
