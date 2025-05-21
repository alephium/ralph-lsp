// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

/**
 * Implements [[CodeProvider]] that provides go-to definition results of type [[SourceLocation.GoToDefStrict]].
 */
private[search] case object GoToDefCodeProvider extends CodeProvider[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict] with StrictImplicitLogging {

  /** @inheritdoc */
  override def searchLocal(
      cursorIndex: Int,
      sourceCode: SourceCodeState.Parsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: GoToDefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefStrict] =
    // find the statement where this cursorIndex sits.
    sourceCode.astStrict.statements.find(_.index contains cursorIndex) match {
      case Some(statement) =>
        statement match {
          case importStatement: Tree.Import =>
            // request is for import go-to definition
            GoToDefImport
              .goTo(
                cursorIndex = cursorIndex,
                dependency = workspace.build.findDependency(DependencyID.Std),
                importStatement = importStatement
              )
              .iterator

          case tree: Tree.Source =>
            // request is for source-code go-to definition
            GoToDefSource.goTo(
              cursorIndex = cursorIndex,
              sourceCode = SourceLocation.CodeStrict(tree, sourceCode),
              workspace = workspace,
              settings = searchSettings
            )
        }

      case None =>
        Iterator.empty
    }

}
