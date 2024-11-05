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

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
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
  def complete(
      cursorIndex: Int,
      dependency: Option[WorkspaceState.Compiled],
      imported: Tree.Import
    )(implicit logger: ClientLogger): ArraySeq[Suggestion.File] =
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

  private def complete(
      cursorIndex: Int,
      dependencySourceCode: ArraySeq[SourceCodeState.Compiled],
      imported: Tree.Import
    )(implicit logger: ClientLogger): ArraySeq[Suggestion.File] =
    dependencySourceCode flatMap {
      compiled =>
        compiled.importIdentifier map {
          dependencyIdentifier =>
            val insert =
              imported.path match {
                case Some(importPath) => // user input import statement has some text
                  if (importPath.file.index contains cursorIndex) // does the cursorIndex belong to text after the forward slash?
                    dependencyIdentifier.path match {
                      case Some(path) => // Yes it does and the path exists.
                        path.file.value // Suggest only the file names e.g. `nft_interface`

                      case None =>
                        // This should never be the case. Dependencies should always have a path e.g. `std/nft_interface`.
                        logger.error(s"Dependency's Import-identifier without path: ${dependencyIdentifier.string.value}")
                        dependencyIdentifier.string.name.value // path does not exist, suggest full importIdentifier
                    }
                  else
                    dependencyIdentifier.string.name.value // else suggest the package and file name e.g. `std/nft_interface`

                case None =>
                  dependencyIdentifier.string.name.value // suggest the package and file name e.g. `std/nft_interface`
              }

            Suggestion.File(insert)
        }
    }

}
