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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

/**
 * Implements [[CodeProvider]] that provides go-to references results of type [[SourceLocation.GoToRef]].
 *
 * To execution this function invoke [[CodeProvider.search]] with [[Boolean]] and [[SourceLocation.GoToRef]] as type parameter.
 */
private[search] case object GoToReferenceProvider extends CodeProvider[GoToRefSetting, SourceLocation.GoToRef] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToRef] =
    // find the statement where this cursorIndex sits.
    sourceCode.astStrict.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import go-to definition
            GoToRefImport
              .goTo(
                cursorIndex = cursorIndex,
                importStatement = importStatement,
                workspace = workspace
              )
              .iterator

          case _: Tree.Source =>
            // request is for source-code go-to definition
            GoToRefSource.goTo(
              cursorIndex = cursorIndex,
              sourceCode = sourceCode,
              workspace = workspace,
              settings = searchSettings
            )

        }

      case None =>
        Iterator.empty
    }

}
