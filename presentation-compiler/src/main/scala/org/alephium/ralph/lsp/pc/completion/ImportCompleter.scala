package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import scala.collection.immutable.ArraySeq

object ImportCompleter {

  /**
   * Perform import code completion within the given dependency.
   *
   * @param cursorIndex The cursor position.
   * @param dependency  The dependency/dependant code to use for code completion.
   * @param imported    The user imputed import statement.
   * @return Import suggestions
   */
  def complete(cursorIndex: Int,
               dependency: Option[WorkspaceState.Compiled],
               imported: Tree.Import): ArraySeq[Suggestion.Field] =
    if (imported.string.name.index contains cursorIndex) // suggest if cursor is between the quoted String
      dependency match {
        case Some(dependency) =>
          complete(
            cursorIndex = cursorIndex,
            sourceCode = dependency.sourceCode,
            imported = imported
          )

        case None =>
          ArraySeq.empty
      }
    else
      ArraySeq.empty

  private def complete(cursorIndex: Int,
                       sourceCode: ArraySeq[SourceCodeState.Compiled],
                       imported: Tree.Import): ArraySeq[Suggestion.Field] =
    sourceCode flatMap {
      compiled =>
        compiled.importIdentifier map {
          importIdentifier =>
            val insert =
              imported.path match {
                case Some(path) => // user input import statement has some text
                  if (path.file.index contains cursorIndex) // does it belong to text after the forward slash?
                    // FIXME: String manipulations like these are not good.
                    //        Temporarily to make the suggestion cleaner, remove package name from suggestions.
                    importIdentifier.replaceFirst(path.folder.value + "/", "") // Yes! Suggest only the file names e.g. `nft_interface`
                  else
                    importIdentifier // else suggest the package and file name e.g. `std/nft_interface`

                case None =>
                  importIdentifier // suggest the package and file name e.g. `std/nft_interface`
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
