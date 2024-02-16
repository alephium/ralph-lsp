package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.util.{StringUtil, URIUtil}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI
import scala.collection.immutable.ArraySeq

object CodeCompleter extends StrictImplicitLogging {

  /**
   * Provides code completion for the cursor position within the current workspace state.
   *
   * @param line      Line position in a document (zero-based).
   * @param character Character offset on a line in a document (zero-based).
   * @param uri       The text document's uri.
   * @param workspace Current workspace state.
   * @return
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               workspace: WorkspaceState.IsSourceAware)(implicit logger: ClientLogger): ArraySeq[Suggestion] =
    // file must belong to the workspace contractURI and must be a ralph source file
    if (URIUtil.contains(workspace.build.contractURI, uri) && URIUtil.getFileExtension(uri) == CompilerAccess.RALPH_FILE_EXTENSION)
      workspace.sourceCode.find(_.fileURI == uri) match {
        case Some(source) =>
          source match {
            case _: SourceCodeState.OnDisk =>
              logger.error(s"Code completion failed: Source code is on-disk and not compiled. URI: $uri")
              ArraySeq.empty

            case _: SourceCodeState.UnCompiled =>
              logger.error(s"Code completion failed: Source code is not compiled. URI: $uri")
              ArraySeq.empty

            case _: SourceCodeState.ErrorAccess =>
              logger.error(s"Code completion failed: Source code errored on access. URI: $uri")
              ArraySeq.empty

            case parsed: SourceCodeState.Parsed =>
              complete(
                line = line,
                character = character,
                workspace = workspace,
                sourceCode = parsed
              )

            case compiled: SourceCodeState.Compiled =>
              complete(
                line = line,
                character = character,
                workspace = workspace,
                sourceCode = compiled.parsed
              )

            case errored: SourceCodeState.ErrorSource =>
              errored.previous match {
                case Some(previousParsed) =>
                  complete(
                    line = line,
                    character = character,
                    workspace = workspace,
                    sourceCode = previousParsed
                  )

                case None =>
                  logger.error(s"Code completion failed: Source code has compilation error(s). URI: $uri")
                  ArraySeq.empty
              }
          }

        case None =>
          logger.error(s"Code completion failed: Source code not found. URI: $uri")
          ArraySeq.empty
      }
    else
      ArraySeq.empty

  private def complete(line: Int,
                       character: Int,
                       workspace: WorkspaceState.IsSourceAware,
                       sourceCode: SourceCodeState.Parsed)(implicit logger: ClientLogger): ArraySeq[Suggestion] = {
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
            ImportCompleter.complete(
              cursorIndex = cursorIndex,
              dependency = workspace.build.dependency,
              imported = importStatement
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
