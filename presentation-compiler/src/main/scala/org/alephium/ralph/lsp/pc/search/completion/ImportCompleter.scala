package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import scala.collection.immutable.ArraySeq

private object ImportCompleter extends StrictImplicitLogging {

  /**
   * Perform import code completion within the given dependency.
   *
   * @param cursorIndex The cursor position.
   * @param dependency  The dependency/dependant code to use for code completion.
   * @param imported    The user imputed import statement.
   * @return Import suggestions
   */
  def complete(cursorIndex: Int, dependency: Option[WorkspaceState.Compiled], imported: Tree.Import)(
      implicit logger: ClientLogger
  ): ArraySeq[Suggestion.Field] =
    if (imported.string.name.index contains cursorIndex) // suggest if cursor is between the quoted String
      dependency match {
        case Some(dependency) =>
          complete(
            cursorIndex = cursorIndex,
            dependencySourceCode = dependency.sourceCode,
            imported = imported
          )

        case None =>
          ArraySeq.empty
      }
    else
      ArraySeq.empty

  private def complete(cursorIndex: Int,
                       dependencySourceCode: ArraySeq[SourceCodeState.Compiled],
                       imported: Tree.Import
                      )(implicit logger: ClientLogger): ArraySeq[Suggestion.Field] =
    dependencySourceCode flatMap {
      compiled =>
        compiled.importIdentifier map {
          dependencyIdentifier =>
            val insert =
              imported.path match {
                case Some(importPath) => // user input import statement has some text
                  if (
                    importPath.file.index contains cursorIndex
                  ) // does the cursorIndex belong to text after the forward slash?
                    dependencyIdentifier.path match {
                      case Some(path) => // Yes it does and the path exists.
                        path.file.value // Suggest only the file names e.g. `nft_interface`

                      case None =>
                        // This should never be the case. Dependencies should always have a path e.g. `std/nft_interface`.
                        logger.error(
                          s"Dependency's Import-identifier without path: ${dependencyIdentifier.string.value}"
                        )
                        dependencyIdentifier.string.name.value // path does not exist, suggest full importIdentifier
                    }
                  else
                    dependencyIdentifier.string.name.value // else suggest the package and file name e.g. `std/nft_interface`

                case None =>
                  dependencyIdentifier.string.name.value // suggest the package and file name e.g. `std/nft_interface`
              }

            Suggestion.Field(
              label = insert,
              insert = insert,
              detail = "",
              documentation = ""
            )
        }
    }
}
