package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader

case class ErrorEmptyErrorsOnDownload(
    downloader: DependencyDownloader,
    index: SourceIndex)
  extends CompilerMessage.Error {

  override def message: String =
    s"Downloader '${downloader.getClass.getSimpleName}' resulted in empty errors."

}
