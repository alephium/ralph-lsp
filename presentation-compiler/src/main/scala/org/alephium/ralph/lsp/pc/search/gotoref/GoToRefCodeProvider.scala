// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

/**
 * Implements [[CodeProvider]] that provides go-to references results of type [[SourceLocation.GoToRefStrict]].
 */
private[search] case object GoToRefCodeProvider extends CodeProvider[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToRefStrict] =
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
