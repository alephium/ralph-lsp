package org.alephium.ralph.lsp.access.file

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}

import java.net.URI
import java.nio.file.Path

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
  def exists(fileURI: URI,
             index: SourceIndex): Either[CompilerMessage.AnyError, Boolean]

  /**
   * Fetch all workspace source file locations.
   *
   * @param workspaceURI Project/workspace location.
   */
  def list(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]]

  /**
   * Fetch the source-code of a file.
   *
   * @param fileURI source-code location.
   */
  def read(fileURI: URI): Either[CompilerMessage.AnyError, String]

  /** Write string to the given file URI. */
  def write(fileURI: URI,
            string: String): Either[CompilerMessage.AnyError, Path]

}
