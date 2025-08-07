// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.completion

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

/**
 * Implements [[CodeProvider]] that provides code completion results of type [[Suggestion]].
 */
private[search] case object CodeCompletionProvider extends CodeProvider[SourceCodeState.Parsed, Unit, Suggestion] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: Unit
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[Suggestion] =
    // find the statement where this cursorIndex sits.
    sourceCode.astStrict.statements.find(_.index contains cursorIndex) match {
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

          case tree: Tree.Source =>
            // request is within a contract source-code
            SourceCodeCompleter.complete(
              cursorIndex = cursorIndex,
              sourceCode = SourceLocation.CodeStrict(tree, sourceCode),
              workspace = workspace
            )
        }

      case None =>
        TopLevelCompleter
          .suggest()
          .iterator
    }

}
