package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

case class ErrorDownloadingDependency(
    dependencyID: String,
    throwable: Throwable,
    index: SourceIndex)
  extends CompilerMessage.Error {

  def title =
    s"Failed to download dependency: $dependencyID"

  override def message: String =
    s"""$title
       |${throwable.getMessage}
       |""".stripMargin

}
