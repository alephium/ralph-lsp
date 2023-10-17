package org.alephium.ralph.lsp.access.file

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage.AnyError

import java.net.URI

object FileAccess {
  // disk file-io
  def disk: FileAccess =
    DiskFileAccess
}

/**
 * Implements functions accessing file IO.
 */
trait FileAccess {

  /**
   * Checks if a source-file exists.
   *
   * @param fileURI source-file location
   */
  def sourceExists(fileURI: URI): Either[AnyError, Boolean]

  /**
   * Fetch all workspace source file locations.
   *
   * @param workspaceURI Project/workspace location.
   */
  def getSourceFiles(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]]

  /**
   * Fetch the source-code of a file.
   *
   * @param fileURI source-code location.
   */
  def getSourceCode(fileURI: URI): Either[CompilerMessage.AnyError, String]

}
