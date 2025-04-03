// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server.converter

import java.net.URI
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{TextEdit, WorkspaceEdit}

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava}

object RenameConverter {

  /** Converts rename results to LSP4J type [[WorkspaceEdit]] */
  def toWorkspaceEdits(
      goTos: ArraySeq[SourceLocation.GoToRename],
      newText: String): WorkspaceEdit = {
    val locations =
      toTextEdits(
        goTos = goTos.iterator,
        newText = newText
      ).map {
        case (key, value) =>
          (key.toString, value.asJava)
      }

    new WorkspaceEdit(locations.asJava)
  }

  /** Converts rename results to LSP4J type [[lsp4j.TextEdit]] */
  private def toTextEdits(
      goTos: Iterator[SourceLocation.GoToRename],
      newText: String): Map[URI, List[lsp4j.TextEdit]] =
    goTos
      .flatMap {
        goTo =>
          toTextEdit(
            goTo = goTo,
            newText = newText
          )
      }
      .toList
      .groupMap(_._1)(_._2)

  /** Converts a rename result to LSP4J type [[lsp4j.TextEdit]] */
  private def toTextEdit(
      goTo: SourceLocation.GoToRename,
      newText: String): Option[(URI, TextEdit)] =
    goTo.toLineRange() map {
      lineRange =>
        val range    = CommonConverter.toRange(lineRange)
        val textEdit = new lsp4j.TextEdit(range, newText)
        val fileURI  = goTo.parsed.fileURI
        (fileURI, textEdit)
    }

}
