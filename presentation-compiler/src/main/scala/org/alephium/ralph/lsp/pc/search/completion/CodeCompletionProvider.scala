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
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

/**
 * Implements [[CodeProvider]] that provides code completion results of type [[Suggestion]].
 *
 * To execution this function invoke [[CodeProvider.search]] with [[Suggestion]] as type parameter.
 */
private[search] object CodeCompletionProvider extends CodeProvider[Suggestion] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[Suggestion] =
    // find the statement where this cursorIndex sits.
    sourceCode.ast.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import statement completion
            ImportCompleter
              .complete(
                cursorIndex = cursorIndex,
                dependency = workspace.build.findDependency(DependencyID.Std),
                imported = importStatement
              )
              .iterator

          case _: Tree.Source =>
            Iterator.empty // TODO: Provide source level completion.
        }

      case None =>
        TopLevelCompleter
          .suggest()
          .iterator
    }

}
