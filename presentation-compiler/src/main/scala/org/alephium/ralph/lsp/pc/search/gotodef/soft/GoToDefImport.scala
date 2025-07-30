// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.soft

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node

import scala.collection.immutable.ArraySeq

object GoToDefImport {

  /**
   * Searches for source files that match the import path specified by given [[SoftAST.Path]]'s,
   * parent node [[SoftAST.StringLiteral]].
   *
   * Attempts to locate the source file with matching paths,
   * accounting for only those paths occurring before the specified `cursorIndex`.
   *
   * @param cursorIndex The index (character offset) in the source code representing the cursor position.
   * @param path        The [[SoftAST.Path]] defined at or near the `cursorIndex`'s position.
   * @param dependency  The dependency workspace to search.
   * @return Matching source files.
   */
  def apply(
      cursorIndex: Int,
      path: Node[SoftAST.Path, SoftAST],
      dependency: Option[WorkspaceState.Compiled]): ArraySeq[SourceLocation.GoToDefSoft] =
    path.parent match {
      case Some(Node(string: SoftAST.StringLiteral, _)) =>
        apply(
          cursorIndex = cursorIndex,
          string = string,
          dependency = dependency
        )

      case _ =>
        ArraySeq.empty
    }

  /**
   * Searches for source files that match the import path specified by the given [[SoftAST.StringLiteral]].
   *
   * Attempts to locate the source file with matching paths,
   * accounting for only those paths occurring before the specified `cursorIndex`.
   *
   * @param cursorIndex The index (character offset) in the source code representing the cursor position.
   * @param string      The [[SoftAST.StringLiteral]] defined at or near the `cursorIndex`'s position.
   * @param dependency  The dependency workspace to search.
   * @return Matching source files.
   */
  def apply(
      cursorIndex: Int,
      string: SoftAST.StringLiteral,
      dependency: Option[WorkspaceState.Compiled]): ArraySeq[SourceLocation.GoToDefSoft] =
    dependency match {
      case Some(dependency) =>
        // Build a path containing all the sub-paths occurring before the cursor-index.
        val pathToSearch =
          string
            .paths
            .takeWhile(_.index.from <= cursorIndex)
            .map(_.text)
            .mkString(Token.ForwardSlash.lexeme)

        // Find all dependency source files that match the selected path
        val matchedSourceFiles =
          dependency.sourceCode.filter {
            depSourceFile =>
              depSourceFile.importIdentifierString.exists {
                depFilePath =>
                  depFilePath startsWith pathToSearch
              }
          }

        matchedSourceFiles map {
          code =>
            SourceLocation.File(code.parsed)
        }

      case None =>
        ArraySeq.empty
    }

}
